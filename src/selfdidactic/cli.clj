(ns selfdidactic.cli
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.cli :as cli]
            [selfdidactic.config :as config]
            [selfdidactic.git :as git]
            [selfdidactic.pipeline :as pipeline]
            [selfdidactic.progress :as progress]
            [selfdidactic.runner :as runner]
            [selfdidactic.timetable :as timetable]
            [selfdidactic.version :as version])
  (:import [java.io IOException]))

(def program-name "switchyard")

;; switchyard's own process exit codes (0 is success). They say what happened to
;; the run, not what the agent returned: an agent's exit code appears only in
;; the failure message and the run record.
(def exit-problem
  "Nothing was run: the configuration could not be loaded, or the pipeline has
  problems (unknown references, bad :writes, a plugin that rejects a step...)."
  1)
(def exit-usage
  "The command line is invalid."
  2)
(def exit-step-failed
  "A valid step ran and its agent process crashed, timed out, or could not be
  started, and left no violations."
  3)
(def exit-violations
  "A step kept breaking its write jurisdiction, or tampering with git, until
  its retries ran out. Violations are checked before the process outcome: an
  attempt with violations is rolled back and retried even when its process
  also failed, and an exhausted retry bound ends in 4, not 3."
  4)

(def cli-options
  [["-h" "--help" "Show this help"]
   ["-v" "--version" "Show program version"]
   ["-s" "--spec PATH" "Path to a Markdown or Org specification document"
    :update-fn (fnil conj [])
    :multi true]
   ["-c" "--config PATH" "path to toml config file"]
   [nil "--validate" "validate configuration and print it in edn format"]
   [nil "--pipeline NAME" "run the named pipeline from the branch's timetable"]
   [nil "--force-unlock" "clear a stale run lock before running"]
   [nil "--[no-]worktree" "run in a new git worktree (the default), or in place"]
   [nil "--run-id ID" "name the run; re-use that run's worktree if it exists"]
   [nil "--allow-dirty" "start although the checkout has uncommitted changes"]
   [nil "--headless" "no progress line on the terminal (log output is unchanged)"]])

(defn- usage
  [summary]
  (str "Usage: " program-name " [options]\n\n"
       "Options:\n"
       summary))

(defn- usage-error
  [summary errors]
  {:kind :usage-error
   :errors errors
   :usage (usage summary)})

(defn- flag-argument-error
  [argument]
  (cond
    (str/starts-with? argument "--help=")
    "Option --help does not accept a value."

    (str/starts-with? argument "--version=")
    "Option --version does not accept a value."))

(defn parse-command
  "Calculate a command data value from command-line arguments."
  [args]
  (let [{:keys [options arguments summary errors]}
        (cli/parse-opts args cli-options :strict true)
        spec-paths (:spec options)
        option-arguments (take-while (complement #{"--"}) args)
        explicit-help? (some #{"-h" "--help"} option-arguments)
        explicit-version? (some #{"-v" "--version"} option-arguments)
        flag-argument-errors (keep flag-argument-error option-arguments)]
    (cond
      explicit-help?
      {:kind :help
       :usage (usage summary)}

      explicit-version?
      {:kind :version}

      (seq flag-argument-errors)
      (usage-error summary flag-argument-errors)

      (:help options)
      {:kind :help
       :usage (usage summary)}

      (:version options)
      {:kind :version}

      (seq errors)
      (usage-error summary errors)

      (seq arguments)
      (usage-error summary
                   [(str "Unexpected argument(s): "
                         (str/join " " arguments))])

      (> (count spec-paths) 1)
      (usage-error summary
                   ["Option --spec may be specified only once."])

      (some str/blank? spec-paths)
      (usage-error summary
                   ["Option --spec requires a non-blank path."])

      :else
      {:kind :run
       :usage (usage summary)
       :no-args? (empty? args)
       :validate? (boolean (:validate options))
       :config-path (:config options)
       :pipeline (:pipeline options)
       :force-unlock? (boolean (:force-unlock options))
       :worktree (:worktree options)
       :run-id (:run-id options)
       :allow-dirty? (boolean (:allow-dirty options))
       :headless? (boolean (:headless options))
       :overrides (if (seq spec-paths) {:spec (first spec-paths)} {})})))

(defn resolve-command
  "Calculate what to do from a parsed :run command and the file config.
  First matching row wins, top to bottom."
  [{:keys [usage no-args? validate? config-path overrides pipeline force-unlock?
           worktree run-id allow-dirty? headless?]}
   file-config]
  (let [config (config/merge-overrides file-config overrides)
        missing (config/missing-keys config/required-keys config)
        described (config/describe-missing config/required-keys missing)
        sources (config/key-sources
                 overrides file-config
                 (config/source-label config-path config/default-resource))]
    (cond
      (and (seq missing) validate?)
      {:kind :missing-keys :message described}

      (and (seq missing) no-args?)
      {:kind :help :usage usage}

      (seq missing)
      {:kind :usage-error :errors [described] :usage usage}

      validate?
      {:kind :validate :config config :path (:spec config)
       :source (:spec sources)}

      (and pipeline (contains? config :worktree) (not (boolean? (:worktree config))))
      {:kind :config-error :message "`worktree` must be true or false in the config file"}

      pipeline
      {:kind :run-pipeline :config config :pipeline-name (keyword pipeline)
       :force-unlock? force-unlock?
       ;; The flag wins over the config file; a worktree is the default.
       :worktree? (if (some? worktree) worktree (get config :worktree true))
       :run-id run-id :allow-dirty? allow-dirty? :headless? headless?}

      ;; Complete config without --pipeline: describe what would run.
      :else
      {:kind :show :config config})))

(defn specification-format
  "Calculate a specification format from a path, or nil when unsupported."
  [path]
  (let [lower-path (str/lower-case path)]
    (cond
      (or (str/ends-with? lower-path ".md")
          (str/ends-with? lower-path ".markdown")) :markdown
      (str/ends-with? lower-path ".org") :org)))

(defn load-specification!
  "Read a supported specification into an explicit data value."
  [path]
  (let [format (specification-format path)
        file (io/file path)]
    (when-not format
      (throw (ex-info "unsupported format; expected .md, .markdown, or .org"
                      {:kind :unsupported-format
                       :path path})))
    (when-not (and (.isFile file) (.canRead file))
      (throw (ex-info "path is not a readable regular file"
                      {:kind :unreadable-file
                       :path path})))
    {:path path
     :format format
     :contents (slurp file :encoding "UTF-8")}))

(defn- usage-error-response
  [{:keys [errors usage]}]
  {:exit-code exit-usage
   :stderr (str "Error:\n"
                (str/join "\n" (map #(str "  " %) errors))
                "\n\n"
                usage)})

(defn- specification-error-response
  [path source message]
  {:exit-code exit-problem
   :stderr (str "Error: unable to read specification '" path
                "' (from " source "): " message)})

(defn- error-response
  [message]
  {:exit-code exit-problem
   :stderr (str "Error: " message)})

(defn- show-response
  [config]
  {:exit-code 0
   :stdout (config/show-str config)})

(defn- validate-response!
  [{:keys [config path source]}]
  (try
    (load-specification! path)
    (show-response config)
    (catch clojure.lang.ExceptionInfo exception
      (specification-error-response path source (.getMessage exception)))
    (catch IOException exception
      (specification-error-response path source (.getMessage exception)))
    (catch SecurityException exception
      (specification-error-response path source (.getMessage exception)))))

;; -- Running a pipeline -------------------------------------------------

(defn- problem-line [p] (str "  " (pr-str p)))

(defn outcome->response
  "Calculate response data from a runner outcome. Success is quiet apart from
  the path of the run's worktree; the agent's own exit code is reported in the
  message, never passed through."
  [{:keys [status problems steps step record worktree]}]
  (let [last-step (last steps)
        where (str "step " step " (" (some-> (:agent last-step) name) ")")
        trailer (str "\nrecord: " record
                     (when worktree (str "\nworktree: " (:path worktree))))]
    (case status
      :success (cond-> {:exit-code 0}
                 worktree (assoc :stdout (str "worktree: " (:path worktree))))
      :problems {:exit-code exit-problem
                 :stderr (str "Error: cannot run pipeline:\n"
                              (str/join "\n" (map problem-line problems)))}
      :step-failed {:exit-code exit-step-failed
                    :stderr (str "Error: " where " failed: " (name (:process last-step))
                                 (some->> (:reason last-step) name (str ", "))
                                 (if-let [code (:exit-code last-step)]
                                   (str " (agent exit code " code ")")
                                   (str " (" (:err last-step) ")"))
                                 trailer)}
      :violations {:exit-code exit-violations
                   :stderr (str "Error: " where
                                (if (:stopped? last-step)
                                  " stopped the run: it changed something outside its worktree; nothing there was rolled back:\n"
                                  " stopped on violations after retries; its changes were rolled back:\n")
                                (str/join "\n" (map problem-line (:violations last-step)))
                                trailer)})))

(defn- progress-display
  "The progress line shows on an interactive terminal unless the run is headless."
  [headless?]
  (if (and (not headless?) (System/console))
    (progress/display System/err)
    progress/silent))

(defn- run-pipeline!
  "Load the branch's timetable and the specification, then run the pipeline."
  [{:keys [config pipeline-name force-unlock? worktree? run-id allow-dirty? headless?]}]
  (let [root   (git/toplevel ".")
        branch (git/branch root)]
    (if-not branch
      {:status :problems :problems [{:problem :detached-head}]}
      (let [dir (pipeline/timetable-dir root branch)]
        (when-not (.isDirectory (io/file dir))
          (throw (ex-info (str "no timetable directory: " dir) {})))
        (runner/run-pipeline!
         {:config config :pipeline-name pipeline-name
          :timetable (timetable/load-dir dir)
          :spec (load-specification! (:spec config))
          :root root :branch branch :dir dir
          :force-unlock? force-unlock?
          :worktree? worktree? :run-id run-id :allow-dirty? allow-dirty?
          :progress (progress-display headless?)})))))

(defn- run-pipeline-response!
  [command]
  (try
    (outcome->response (run-pipeline! command))
    (catch clojure.lang.ExceptionInfo exception
      (error-response (str (.getMessage exception)
                           (when-let [ps (:problems (ex-data exception))]
                             (str "\n" (str/join "\n" (map problem-line ps)))))))
    (catch IOException exception
      (error-response (.getMessage exception)))))

(defn execute-command!
  "Perform a command and return response data."
  [{:keys [kind usage config message] :as command}]
  (case kind
    :help {:exit-code 0
           :stdout usage}
    :version {:exit-code 0
              :stdout (str program-name " " version/program-version)}
    :usage-error (usage-error-response command)
    :missing-keys (error-response message)
    :config-error (error-response message)
    :show (show-response config)
    :validate (validate-response! command)
    :run-pipeline (run-pipeline-response! command)))

(defn emit-response!
  "Write response data to the appropriate output streams."
  [{:keys [stdout stderr]}]
  (when stdout
    (println stdout)
    (flush))
  (when stderr
    (binding [*out* *err*]
      (println stderr)
      (flush))))

(defn- resolve-configured!
  "Load the file config for a :run command, then resolve it."
  [{:keys [config-path] :as command}]
  (try
    (resolve-command command (config/load-config! config-path))
    (catch clojure.lang.ExceptionInfo exception
      {:kind :config-error
       :message (str "unable to load config "
                     (config/source-label config-path config/default-resource)
                     ": " (.getMessage exception))})))

(defn run-cli!
  "Run the CLI without terminating the JVM, returning its exit code."
  [args]
  (let [command (parse-command args)
        response (-> (if (= :run (:kind command))
                       (resolve-configured! command)
                       command)
                     execute-command!)]
    (emit-response! response)
    (:exit-code response)))
