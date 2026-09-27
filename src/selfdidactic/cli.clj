(ns selfdidactic.cli
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.cli :as cli]
            [selfdidactic.config :as config]
            [selfdidactic.version :as version])
  (:import [java.io IOException]))

(def program-name "switchyard")

(def cli-options
  [["-h" "--help" "Show this help"]
   ["-v" "--version" "Show program version"]
   ["-s" "--spec PATH" "Path to a Markdown or Org specification document"
    :update-fn (fnil conj [])
    :multi true]
   ["-c" "--config PATH" "path to toml config file"]
   [nil "--validate" "validate configuration and print it in edn format"]])

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
       :overrides (if (seq spec-paths) {:spec (first spec-paths)} {})})))

(defn resolve-command
  "Calculate what to do from a parsed :run command and the file config.
  First matching row wins, top to bottom."
  [{:keys [usage no-args? validate? config-path overrides]} file-config]
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

      ;; Complete config: describe what would run. The next module replaces
      ;; this branch with the real action.
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
  {:exit-code 2
   :stderr (str "Error:\n"
                (str/join "\n" (map #(str "  " %) errors))
                "\n\n"
                usage)})

(defn- specification-error-response
  [path source message]
  {:exit-code 1
   :stderr (str "Error: unable to read specification '" path
                "' (from " source "): " message)})

(defn- error-response
  [message]
  {:exit-code 1
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
    :validate (validate-response! command)))

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
