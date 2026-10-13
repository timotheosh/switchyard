(ns selfdidactic.runner
  "Run one pipeline: the actions around the calculations in
  selfdidactic.pipeline (lock, run record, processes, git)."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [selfdidactic.git :as git]
            [selfdidactic.harness :as harness]
            [selfdidactic.pipeline :as pipeline]
            [selfdidactic.plugins :as plugins])
  (:import [java.io File]
           [java.net InetAddress]
           [java.nio.file FileAlreadyExistsException Files OpenOption
            StandardOpenOption]
           [java.security MessageDigest]
           [java.time Instant ZoneOffset ZonedDateTime]
           [java.time.format DateTimeFormatter]))

;; -- Small actions ------------------------------------------------------

(defn- hostname [] (.getHostName (InetAddress/getLocalHost)))

(defn- pid-alive? [pid] (.isPresent (java.lang.ProcessHandle/of (long pid))))

(defn- sha256 [^String s]
  (apply str (map #(format "%02x" %)
                  (.digest (MessageDigest/getInstance "SHA-256") (.getBytes s "UTF-8")))))

(defn- run-id []
  (str (.format (DateTimeFormatter/ofPattern "yyyyMMdd'T'HHmmssSSS'Z'")
                (ZonedDateTime/now ZoneOffset/UTC))
       "-" (format "%06x" (rand-int 0xffffff))))

(defn- append!
  "Append one entry to the run record. Only the runner writes it."
  [^File record entry]
  (spit record (prn-str entry) :append true))

;; -- Lock ---------------------------------------------------------------

(defn acquire-lock!
  "Create the run lock atomically; {:lock file} or {:problem ...}. A held lock
  is never cleared unless force? is set."
  [dir force?]
  (let [f    (io/file dir "lock")
        host (hostname)
        data {:pid (.pid (java.lang.ProcessHandle/current)) :host host
              :since (str (Instant/now))}]
    (when force? (.delete f))
    (try
      (Files/writeString (.toPath f) (pr-str data)
                         (into-array OpenOption [StandardOpenOption/CREATE_NEW
                                                 StandardOpenOption/WRITE]))
      {:lock f}
      (catch FileAlreadyExistsException _
        (let [held (try (edn/read-string (slurp f)) (catch Exception _ {}))]
          {:problem (pipeline/lock-problem
                     held host (and (map? held) (:pid held) (pid-alive? (:pid held))))})))))

;; -- Writes directories -------------------------------------------------

(defn inspect-writes
  "Read what exists on disk for each prefix: {prefix :dir|:file|:missing}."
  [root writes]
  (into (sorted-map)
        (for [p writes :let [f (io/file root p)]]
          [p (cond (.isDirectory f) :dir (.exists f) :file :else :missing)])))

(defn ensure-dirs!
  "Create each missing writes directory; return the prefixes created. Git
  cannot see an empty directory, so this also restores one an agent removed
  or a rollback pruned."
  [root writes]
  (vec (for [p writes :let [f (io/file root p)] :when (not (.isDirectory f))]
         (do (.mkdirs f)
             (when-not (.isDirectory f)
               (throw (ex-info (str "cannot create writes directory " p)
                               {:kind :writes-dir :prefix p})))
             p))))

;; -- Prompts ------------------------------------------------------------

(defn read-prompts
  "Read every prompt the pipeline uses: {prompt-name {:text s} | {:error s}}.
  Paths are relative to the timetable directory."
  [dir timetable pipeline-name]
  (into {}
        (for [p (distinct (keep :prompt (get-in timetable [:pipelines pipeline-name :steps])))
              :let [path (get-in timetable [:prompts p :path])] :when path
              :let [f (io/file dir path)]]
          [p (if (.isFile f)
               {:text (slurp f)}
               {:error (str "not a readable file: " f)})])))

;; -- Steps --------------------------------------------------------------

(defn- run-attempt!
  "One attempt: pre-snapshot, agent, post-snapshot, violations, restore when
  needed, then append the attempt to the record. The record is written after
  the restore so a tampering agent cannot roll back the runner's own entry."
  [{:keys [root timetable writes] :as ctx} i step n context record]
  (let [agent (get-in timetable [:agents (:agent step)])
        harness (get-in timetable [:harnesses (:harness agent)])
        {:keys [command problem]} (pipeline/step-command ctx i step context)
        before (git/snapshot root)
        run (if problem
              {:process :harness-error :exit-code nil :out "" :err (:message problem)}
              (harness/start (:argv command) root
                             {:stdin (:stdin command)
                              :env {"GIT_DIR" "/nonexistent"}
                              :exit-reasons (plugins/exit-reasons (:config ctx) harness)
                              :timeout-ms (or (:timeout-ms agent)
                                              pipeline/default-timeout-ms)}))
        after (git/snapshot root)
        changed (git/changed-paths root before after)
        parsed (pipeline/parse-agent-output (:out run))
        violations (pipeline/step-violations agent before after changed parsed)
        _ (when (seq violations) (git/restore! root before after))
        recreated (ensure-dirs! root writes)
        entry (merge run
                     {:entry :attempt :step i :agent (:agent step) :attempt n
                      :argv (:argv command) :stdin (:stdin command)
                      :before before :after after :changed changed
                      :writes-trees {:before (git/dir-trees root before writes)
                                     :after  (git/dir-trees root after writes)}
                      :recreated-dirs recreated
                      :verdict :none :violations violations
                      :restored? (boolean (seq violations))
                      :needs-outside-jurisdiction
                      (get-in parsed [:output :needs-outside-jurisdiction] [])})]
    (append! record entry)
    entry))

(defn- run-step!
  "Run a step, retrying it after a rollback while violations persist and the
  agent's retry bound allows. Returns the last attempt with :step-status."
  [ctx i step record]
  (let [agent (get-in (:timetable ctx) [:agents (:agent step)])
        bound (or (:max-violation-retries agent)
                  pipeline/default-max-violation-retries)]
    (loop [n 0 context {}]
      (let [entry (run-attempt! ctx i step n context record)]
        (cond
          (empty? (:violations entry))
          (assoc entry :step-status (if (pipeline/step-continues? entry) :ok :step-failed))

          (< n bound)
          (recur (inc n) {:violations (:violations entry) :writes (:writes agent)})

          :else
          (assoc entry :step-status :violations))))))

(defn- finish!
  [record status steps step-index]
  (append! record {:entry :terminal :status status :step step-index})
  {:status status :step step-index :steps steps :record (str record)})

(defn- execute!
  [{:keys [dir root timetable pipeline-name spec writes] :as ctx}]
  (let [record  (io/file dir "runs" (str (run-id) ".edn"))
        steps   (get-in timetable [:pipelines pipeline-name :steps])
        created (ensure-dirs! root writes)]
    (io/make-parents record)
    (append! record {:entry :header :pipeline pipeline-name
                     :writes writes :created-dirs created
                     :spec-path (:path spec)
                     :spec-sha256 (sha256 (:contents spec))
                     :timetable-sha256 (sha256 (pr-str (pipeline/canonical timetable)))})
    (loop [i 0 done []]
      (if (= i (count steps))
        (finish! record :success done nil)
        (let [entry (run-step! ctx i (nth steps i) record)
              done  (conj done entry)]
          (if (= :ok (:step-status entry))
            (recur (inc i) done)
            (finish! record (:step-status entry) done i)))))))

(defn run-pipeline!
  "Run the named pipeline sequentially. ctx: :config :timetable
  :pipeline-name :spec {:path :contents} :root (repo top level) :branch
  :dir (the branch's timetable directory) :force-unlock?.
  Returns {:status :success|:step-failed|:violations ...} or
  {:status :problems :problems [...]} before anything has run."
  [{:keys [dir root timetable pipeline-name force-unlock?] :as ctx}]
  (let [writes   (pipeline/writes-list timetable pipeline-name)
        ctx      (assoc ctx
                        :writes writes
                        :writes-fs (if root (inspect-writes root writes) {})
                        :prompt-texts (if dir (read-prompts dir timetable pipeline-name) {}))
        problems (pipeline/pipeline-problems ctx)]
    (if (seq problems)
      {:status :problems :problems problems}
      (let [{:keys [lock problem]} (acquire-lock! dir force-unlock?)]
        (if problem
          {:status :problems :problems [problem]}
          (try (execute! ctx)
               (finally (.delete ^File lock))))))))
