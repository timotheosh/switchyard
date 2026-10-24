(ns selfdidactic.pipeline
  "Calculations for running a pipeline: validation as data, prompt
  composition, jurisdiction and output checks. Nothing here touches the
  filesystem, git or processes; selfdidactic.runner performs those actions."
  (:require [clojure.edn :as edn]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [selfdidactic.git :as git]
            [selfdidactic.plugins :as plugins]))

(def max-argv-bytes
  "Linux caps one argv string (MAX_ARG_STRLEN) at 128 KiB."
  (* 128 1024))

(def default-timeout-ms (* 30 60 1000))

(def default-max-violation-retries 2)

(def step-keys #{:agent :prompt})

;; -- Branch directory ---------------------------------------------------

(defn encode-branch
  "Calculate an injective directory name for a branch: `%` then `/` are
  percent-encoded, so feat/a-b and feat-a/b never collide."
  [branch]
  (-> branch (str/replace "%" "%25") (str/replace "/" "%2F")))

(defn timetable-dir
  "Calculate the per-branch directory that holds the timetable and run state."
  [root branch]
  (str root "/.switchyard/" (encode-branch branch)))

;; -- Run worktrees ------------------------------------------------------

(def worktrees-dir
  "The directory under .switchyard/ that holds run worktrees; a branch whose
  encoded name equals it would collide."
  "worktrees")

(defn valid-run-id?
  "A run id becomes a path and a branch component, so it is restricted."
  [id]
  (boolean (and (string? id) (re-matches #"[A-Za-z0-9][A-Za-z0-9._-]*" id))))

(defn worktree-name
  "Calculate the worktree directory name for a run."
  [encoded-branch run-id]
  (str encoded-branch "-" run-id))

(defn worktree-branch
  "Calculate the git branch a run's worktree is created on."
  [encoded-branch run-id]
  (str "switchyard/" encoded-branch "/" run-id))

(defn worktree-problems
  "Calculate the problems that stop a worktree run from being set up. ctx:
  :worktree? :branch :run-id :allow-dirty? :dirty-paths and :worktree-facts
  ({:registered? :dir-exists? :branch-exists? :ref-conflicts}). A worktree that
  is already registered is re-used, so it skips the conflict and dirty checks."
  [{:keys [worktree? branch run-id allow-dirty? dirty-paths worktree-facts]}]
  (when worktree?
    (let [{:keys [registered? dir-exists? branch-exists? ref-conflicts]} worktree-facts]
      (concat
       (when (and branch (= worktrees-dir (encode-branch branch)))
         [{:problem :reserved-branch-name :branch branch}])
       (when-not (valid-run-id? run-id)
         [{:problem :invalid-run-id :run-id run-id}])
       (when (and (valid-run-id? run-id) (not registered?))
         (concat
          (when dir-exists?
            [{:problem :worktree-conflict :reason :unregistered-directory}])
          (when branch-exists?
            [{:problem :worktree-conflict :reason :branch-without-worktree}])
          (for [r ref-conflicts]
            {:problem :worktree-conflict :reason :ref-conflict :ref r})))
       (when (and (seq dirty-paths) (not allow-dirty?) (not registered?))
         [{:problem :dirty-checkout :paths (vec dirty-paths)}])))))

;; -- Prompt composition -------------------------------------------------

(defn render-context
  "Calculate the prompt section for a step's context, or nil when empty. v1
  context carries only the violations of a rejected attempt."
  [{:keys [violations writes]}]
  (when (seq violations)
    (str "## Previous attempt rolled back\n\n"
         "Your previous attempt was rolled back because it broke your "
         "jurisdiction:\n"
         (str/join "\n" (map #(str "- " (pr-str %)) violations))
         "\n\nYou may write only under: " (str/join ", " (map pr-str writes))
         ". Nothing else is allowed, including git operations and "
         ".switchyard/. If the work needs a change outside your "
         "jurisdiction, do not make it; end your output with an ```edn "
         "block containing {:needs-outside-jurisdiction [{:path \"..\" "
         ":problem \"..\" :evidence \"..\"}]}.")))

(defn compose-prompt
  "Calculate the prompt text for one step attempt."
  [{:keys [prompt-text spec context]}]
  (str/join "\n\n"
            (remove nil? [prompt-text
                          (str "## Specification (" (:path spec) ")\n\n"
                               (:contents spec))
                          (render-context context)])))

(defn step-command
  "Calculate {:command {:argv :stdin}} for a step, or {:problem ...} when the
  plugin rejects it (e.g. skills opencode cannot take). Never throws."
  [{:keys [config timetable prompt-texts spec]} i step context]
  (let [agent   (get-in timetable [:agents (:agent step)])
        harness (get-in timetable [:harnesses (:harness agent)])
        text    (get-in prompt-texts [(:prompt step) :text])]
    (try
      {:command (plugins/build-command!
                 config harness agent
                 (compose-prompt {:prompt-text text :spec spec :context context}))}
      (catch clojure.lang.ExceptionInfo e
        {:problem {:problem :command :step i :message (.getMessage e)}}))))

;; -- Writes list --------------------------------------------------------

(defn writes-list
  "Calculate the directory prefixes the pipeline's agents may write: the
  sorted, distinct :writes of every agent a step uses. Known before anything
  runs, so directories can be created and protected ahead of time."
  [timetable pipeline-name]
  (->> (get-in timetable [:pipelines pipeline-name :steps])
       (keep :agent)
       distinct
       (mapcat #(get-in timetable [:agents % :writes]))
       distinct
       sort
       vec))

;; -- Problems as data ---------------------------------------------------

(defn- prefix-problem [agent-name prefix]
  (when (or (not (str/ends-with? prefix "/"))
            (str/starts-with? prefix "/")
            (str/includes? prefix "..")
            (str/starts-with? prefix ".switchyard"))
    {:problem :bad-writes-prefix :agent agent-name :prefix prefix}))

(defn- overlaps? [a b]
  (or (str/starts-with? a b) (str/starts-with? b a)))

(defn overlapping-writes
  "Calculate the problems for different agents whose :writes prefixes overlap."
  [agents]
  (let [v (vec (sort-by (comp str key) agents))]
    (for [i (range (count v))
          j (range (inc i) (count v))
          :let [[n1 a1] (v i) [n2 a2] (v j)]
          p1 (:writes a1)
          p2 (:writes a2)
          :when (overlaps? p1 p2)]
      {:problem :overlapping-writes :agents [n1 n2] :prefixes [p1 p2]})))

(defn- reference-problems [timetable prompt-texts i step]
  (let [agent-name (:agent step)
        agent      (get-in timetable [:agents agent-name])
        prompt     (:prompt step)]
    (concat
     (for [k (keys step) :when (not (step-keys k))]
       {:problem :unsupported-step-key :step i :key k})
     (when-not agent
       [{:problem :unknown-agent :step i :agent agent-name}])
     (when (and agent (not (get-in timetable [:harnesses (:harness agent)])))
       [{:problem :unknown-harness :step i :harness (:harness agent)}])
     (cond
       (nil? prompt)
       [{:problem :missing-prompt :step i}]
       (not (get-in timetable [:prompts prompt]))
       [{:problem :unknown-prompt :step i :prompt prompt}]
       (:error (get prompt-texts prompt))
       [{:problem :unreadable-prompt :step i :prompt prompt
         :message (:error (get prompt-texts prompt))}]))))

(defn- agent-problems [timetable steps]
  (let [names  (distinct (keep :agent steps))
        agents (into {} (for [n names :let [a (get-in timetable [:agents n])] :when a]
                          [n a]))]
    (concat
     (for [[n a] agents :when (nil? (:writes a))]
       {:problem :missing-writes :agent n})
     (for [[n a] agents, p (:writes a) :let [prob (prefix-problem n p)] :when prob]
       prob)
     (overlapping-writes agents))))

(defn- command-problems
  "Calculate the plugin's problems, for steps whose references are sound."
  [ctx i step]
  (let [{:keys [timetable prompt-texts]} ctx]
    (when (empty? (reference-problems timetable prompt-texts i step))
      (let [{:keys [command problem]} (step-command ctx i step {})]
        (cond
          problem [problem]
          (and (nil? (:stdin command))
               (> (reduce + (map #(alength (.getBytes ^String % "UTF-8")) (:argv command)))
                  max-argv-bytes))
          [{:problem :argv-too-long :step i}])))))

(defn lock-problem
  "Calculate the problem for a held lock: stale when its holder is a dead
  process on this host, otherwise a run in progress."
  [lock this-host pid-alive?]
  (let [lock (if (map? lock) lock {})]
    (if (and (= this-host (:host lock)) (:pid lock) (not pid-alive?))
      {:problem :stale-lock :lock lock
       :message "holder is not running; remove it with --force-unlock"}
      {:problem :run-in-progress :lock lock})))

(defn pipeline-problems
  "Calculate every problem that makes a pipeline unrunnable, as data (empty
  when runnable). ctx: :config :timetable :pipeline-name :branch :spec
  :prompt-texts, and :writes-fs ({prefix :dir|:file|:missing}, what exists on
  disk for each of the writes list)."
  [{:keys [timetable pipeline-name branch prompt-texts writes-fs] :as ctx}]
  (let [steps (get-in timetable [:pipelines pipeline-name :steps])]
    (vec
     (concat
      (when-not branch [{:problem :detached-head}])
      (worktree-problems ctx)
      (for [[prefix kind] (sort-by key writes-fs) :when (= :file kind)]
        {:problem :writes-not-directory :prefix prefix})
      (if-not steps
        [{:problem :unknown-pipeline :pipeline pipeline-name}]
        (concat
         (mapcat (fn [i step] (reference-problems timetable prompt-texts i step))
                 (range) steps)
         (agent-problems timetable steps)
         (mapcat (fn [i step] (command-problems ctx i step)) (range) steps)))))))

;; -- Stage log messages -------------------------------------------------

(defn stage-started-message
  "Calculate the log message for the start of stage `index` of `total`."
  [index total agent]
  (str "stage " (inc index) "/" total " " (name agent) " started"))

(defn stage-finished-message
  "Calculate the log message for the end of a stage; status is its :step-status."
  [index total agent status]
  (str "stage " (inc index) "/" total " " (name agent) " finished: " (name status)))

;; -- Violations ---------------------------------------------------------

(defn jurisdiction-violations
  "Calculate the violations for changed paths outside the writable prefixes."
  [writes changed]
  (vec (for [p changed :when (not-any? #(str/starts-with? p %) writes)]
         {:violation :out-of-jurisdiction :path p})))

(s/def ::problem string?)
(s/def ::evidence string?)
(s/def ::justification string?)
(s/def ::required-outcome string?)
(s/def ::path string?)
(s/def ::finding (s/keys :req-un [::problem ::evidence ::justification
                                  ::required-outcome]))
(s/def ::findings (s/coll-of ::finding :kind vector?))
(s/def ::outside (s/keys :req-un [::path ::problem ::evidence]))
(s/def ::needs-outside-jurisdiction (s/coll-of ::outside :kind vector?))

(def ^:private edn-fence #"(?s)```edn\s*\n(.*?)```")

(defn parse-agent-output
  "Calculate {:output map} from the last ```edn block of an agent's stdout
  ({} when it has none), or {:malformed reason} when the block is unreadable."
  [out]
  (if-let [[_ body] (last (re-seq edn-fence out))]
    (try (let [v (edn/read-string body)]
           (if (map? v) {:output v} {:malformed "edn block is not a map"}))
         (catch Exception e {:malformed (.getMessage e)}))
    {:output {}}))

(defn output-violations
  "Calculate the :malformed-output violations of an agent's parsed output."
  [agent {:keys [output malformed]}]
  (let [bad (fn [why] [{:violation :malformed-output :reason why}])]
    (cond
      malformed (bad malformed)
      (and (= :findings (:expects agent))
           (not (s/valid? ::findings (:findings output))))
      (bad "expected {:findings [{:problem :evidence :justification :required-outcome}]}")
      (and (contains? output :needs-outside-jurisdiction)
           (not (s/valid? ::needs-outside-jurisdiction
                          (:needs-outside-jurisdiction output))))
      (bad "invalid :needs-outside-jurisdiction"))))

(defn step-violations
  "Calculate all violations of one step attempt."
  [agent before after changed parsed]
  (vec (concat (jurisdiction-violations (:writes agent) changed)
               (git/tamper-violations before after)
               (when (and (:must-change agent) (empty? changed))
                 [{:violation :no-work}])
               (output-violations agent parsed))))

(def stop-kinds
  "Violations that end the run instead of rolling the step back: they happen
  outside the agent's worktree, where a rollback could destroy your own work
  (the main checkout) or is meaningless (the shared git directory)."
  #{:main-checkout-modified :git-dir-modified})

(defn stop-violation? [v] (contains? stop-kinds (:violation v)))

(defn watcher-violations
  "Calculate the violations seen outside the worktree. ctx: :state-changed and
  :main-changed (paths), :git-dir-before and :git-dir-after ({path hash}).
  Changes to the runner's own state are out of jurisdiction like any other
  path and are restored; the others stop the run."
  [{:keys [state-changed main-changed git-dir-before git-dir-after]}]
  (vec (concat
        (for [p state-changed] {:violation :out-of-jurisdiction :path p})
        (for [p main-changed] {:violation :main-checkout-modified :path p})
        (for [p (sort (distinct (concat (keys git-dir-before) (keys git-dir-after))))
              :when (not= (get git-dir-before p) (get git-dir-after p))]
          {:violation :git-dir-modified :path (str ".git/" p)}))))

(defn step-continues?
  "A step lets the run continue only on a clean process outcome and no
  violations. A verdict is never a failure."
  [{:keys [process violations]}]
  (and (= :ok process) (empty? violations)))

(defn canonical
  "Calculate the value with every map sorted, so its printed form is stable."
  [x]
  (walk/postwalk
   (fn [v] (if (map? v) (into (sorted-map-by #(compare (str %1) (str %2))) v) v))
   x))
