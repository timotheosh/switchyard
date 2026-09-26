(ns selfdidactic.switchyard
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.cli :as cli]
            [selfdidactic.version :as version])
  (:import [java.io IOException]))

(def program-name "switchyard")

(def cli-options
  [["-h" "--help" "Show this help"]
   ["-v" "--version" "Show program version"]
   ["-s" "--spec PATH" "Path to a Markdown or Org specification document"
    :update-fn (fnil conj [])
    :multi true]])

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
      (or (empty? args) explicit-help?)
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

      (empty? spec-paths)
      (usage-error summary
                   ["Option --spec is required."])

      (str/blank? (first spec-paths))
      (usage-error summary
                   ["Option --spec requires a non-blank path."])

      :else
      {:kind :spec
       :path (first spec-paths)})))

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
  [path message]
  {:exit-code 1
   :stderr (str "Error: unable to read specification '" path "': " message)})

(defn execute-command!
  "Perform a parsed command and return response data."
  [{:keys [kind path usage] :as command}]
  (case kind
    :help {:exit-code 0
           :stdout usage}
    :version {:exit-code 0
              :stdout (str program-name " " version/program-version)}
    :usage-error (usage-error-response command)
    :spec (try
            (load-specification! path)
            {:exit-code 0}
            (catch clojure.lang.ExceptionInfo exception
              (specification-error-response path (.getMessage exception)))
            (catch IOException exception
              (specification-error-response path (.getMessage exception)))
            (catch SecurityException exception
              (specification-error-response path (.getMessage exception))))))

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

(defn run-cli!
  "Run the CLI without terminating the JVM, returning its exit code."
  [args]
  (let [response (-> args parse-command execute-command!)]
    (emit-response! response)
    (:exit-code response)))

(defn -main
  "Run the non-interactive command-line interface."
  [& args]
  (let [exit-code (run-cli! args)]
    (when-not (zero? exit-code)
      (System/exit exit-code))))
