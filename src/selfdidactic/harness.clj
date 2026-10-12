(ns selfdidactic.harness
  "Run a harness command as a subprocess and classify the outcome."
  (:import [java.io File]
           [java.lang ProcessBuilder$Redirect]
           [java.util.concurrent TimeUnit]))

(defn classify
  "Calculate the process outcome of a finished run. A killed process (signal
  exit) is :crashed like any other non-zero exit; the exit code is kept."
  [{:keys [exit out err timed-out?]}]
  {:process (cond timed-out?      :timeout
                  (zero? exit)    :ok
                  :else           :crashed)
   :exit-code exit :out out :err err})

(defn launch-failure
  "Calculate the outcome of a command that could not be started."
  [exception]
  {:process :harness-error :exit-code nil :out "" :err (.getMessage exception)})

(defn- builder ^ProcessBuilder [argv dir env in out err]
  (let [pb (ProcessBuilder. ^java.util.List (vec argv))]
    (.directory pb (File. ^String dir))
    (doseq [[k v] env] (.put (.environment pb) k v))
    (.redirectInput pb (if in (ProcessBuilder$Redirect/from in) ProcessBuilder$Redirect/INHERIT))
    (.redirectOutput pb (ProcessBuilder$Redirect/to out))
    (.redirectError pb (ProcessBuilder$Redirect/to err))
    pb))

(defn- kill-tree!
  "Kill the descendants first (they would be orphaned, and keep writing, once
  their parent dies), then the process; return only once all are dead."
  [^Process process]
  (let [handles (-> process .descendants .iterator iterator-seq vec)]
    (doseq [h handles] (.destroyForcibly ^java.lang.ProcessHandle h))
    (.destroyForcibly process)
    (.waitFor process)
    (doseq [h handles] (.get (.onExit ^java.lang.ProcessHandle h)))))

(defn- await-exit!
  "Wait for the process; kill its tree on timeout. Return timed-out?."
  [^Process process timeout-ms]
  (let [finished? (if timeout-ms
                    (.waitFor process (long timeout-ms) TimeUnit/MILLISECONDS)
                    (do (.waitFor process) true))]
    (when-not finished? (kill-tree! process))
    (not finished?)))

(defn- run-in-files!
  "Run the builder's process with stdin from `in` (or nil), output captured in
  the temp files, so a chatty child can never block on a full pipe."
  [argv dir {:keys [env timeout-ms]} ^File in ^File out ^File err]
  (let [process (.start (builder argv dir env in out err))
        timed-out? (await-exit! process timeout-ms)]
    (classify {:exit (.exitValue process) :out (slurp out) :err (slurp err)
               :timed-out? timed-out?})))

(defn start
  "Run argv in dir and return its classified result. Options: :stdin text for
  the child, :env extra environment, :timeout-ms (none when nil). A command
  that cannot be started is a :harness-error result."
  ([argv dir] (start argv dir {}))
  ([argv dir {:keys [stdin] :as options}]
   (when (nil? dir)
     (throw (ex-info "start requires a working directory" {:dir dir})))
   (let [out (File/createTempFile "switchyard-out-" ".txt")
         err (File/createTempFile "switchyard-err-" ".txt")
         in  (when stdin (doto (File/createTempFile "switchyard-in-" ".txt")
                           (spit stdin)))]
     (try
       (run-in-files! argv dir options in out err)
       (catch java.io.IOException e (launch-failure e))
       (finally
         (doseq [f [out err in] :when f] (.delete ^File f)))))))
