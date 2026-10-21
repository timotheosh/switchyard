(ns selfdidactic.runner-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [selfdidactic.git :as git]
            [selfdidactic.git-test :as gt]
            [selfdidactic.pipeline :as pipeline]
            [selfdidactic.runner :as sut]
            [selfdidactic.timetable :as tt])
  (:import [java.net InetAddress]))

(defn- setup!
  "A repo with a branch dir holding one prompt and no lock."
  []
  (let [root (gt/make-repo)
        dir  (str root "/.switchyard/main")]
    (.delete (io/file dir "lock"))
    (gt/put! root ".switchyard/main/p.md" "PROMPT")
    {:root root :dir dir}))

(defn- run-it!
  [{:keys [root dir]} agents steps & [extra]]
  (sut/run-pipeline!
   (merge {:config {:plugin "fake"}
           :timetable {:harnesses {:fake {:command "sh"}
                                  :ghost {:command "/definitely/not/here"}}
                       :agents agents
                       :prompts {:p {:path "p.md"}}
                       :pipelines {:dev {:steps steps}}}
           :pipeline-name :dev
           :spec {:path "s.md" :contents "SPEC"}
           :root root :branch "main" :dir dir}
          extra)))

(defn- entries [result]
  (mapv edn/read-string (str/split-lines (slurp (:record result)))))

(defn- attempts [result] (filter #(= :attempt (:entry %)) (entries result)))

(defn- outside-switchyard
  "Paths changed since a snapshot, ignoring runner-owned .switchyard/."
  [root before]
  (remove #(str/starts-with? % ".switchyard/")
          (git/changed-paths root before (git/snapshot root))))

(def step-tw {:agent :tw :prompt :p})
(def step-impl {:agent :impl :prompt :p})

(defn- script-agent [script & {:as more}]
  (merge {:harness :fake :model script :writes ["src/"]} more))

(deftest clean-run
  (let [env (setup!)
        r   (run-it! env {:impl (script-agent "cat >/dev/null; echo new > src/new.clj")} [step-impl])]
    (is (= :success (:status r)))
    (is (= "new\n" (slurp (io/file (:root env) "src/new.clj"))))
    (testing "the record has a header, each attempt, and a terminal entry, in order"
      (is (= [:header :attempt :terminal] (map :entry (entries r))))
      (is (= {:pipeline :dev :spec-path "s.md"} (select-keys (first (entries r)) [:pipeline :spec-path]))))
    (testing "the agent got the prompt and spec on stdin"
      (is (= "PROMPT\n\n## Specification (s.md)\n\nSPEC" (:stdin (first (attempts r))))))
    (testing "the lock is released"
      (is (not (.exists (io/file (:dir env) "lock")))))))

(deftest problems-stop-everything-before-any-process
  (let [env (setup!)
        r   (run-it! env {:impl (script-agent "echo ran > src/ran")} [{:agent :ghost :prompt :p}])]
    (is (= :problems (:status r)))
    (is (not (.exists (io/file (:root env) "src/ran"))))
    (is (not (.exists (io/file (:dir env) "runs"))))))

(deftest process-failure-stops-the-run
  (let [env (setup!)
        r   (run-it! env {:tw   (script-agent "cat >/dev/null; exit 3" :writes ["test/"])
                       :impl (script-agent "echo second > src/second")}
                  [step-tw step-impl])]
    (is (= :step-failed (:status r)))
    (is (= 0 (:step r)))
    (is (= [:crashed] (map :process (attempts r))))
    (is (= 3 (:exit-code (first (attempts r)))))
    (is (= :boom (:reason (first (attempts r)))) "named by the plugin's exit-reasons")
    (is (not (.exists (io/file (:root env) "src/second"))) "later steps never start")))

(deftest timeout-stops-the-run
  (let [env (setup!)
        r   (run-it! env {:impl (script-agent "sleep 5" :timeout-ms 300)} [step-impl])]
    (is (= :step-failed (:status r)))
    (is (= [:timeout] (map :process (attempts r))))))

(deftest out-of-jurisdiction-is-rolled-back-and-bounded
  (let [env    (setup!)
        before (git/snapshot (:root env))
        r (run-it! env {:impl (script-agent "cat >/dev/null; echo x > deps.edn; echo ok > src/ok.clj"
                                  :max-violation-retries 1)
                     :tw   (script-agent "echo second > test/second" :writes ["test/"])}
                [step-impl step-tw])]
    (is (= :violations (:status r)))
    (is (= 0 (:step r)))
    (testing "the whole step is rolled back, in-jurisdiction changes included"
      (is (empty? (outside-switchyard (:root env) before)))
      (is (not (.exists (io/file (:root env) "deps.edn"))))
      (is (not (.exists (io/file (:root env) "src/ok.clj")))))
    (testing "every attempt is recorded, each with its violations"
      (is (= [0 1] (map :attempt (attempts r))))
      (is (every? #(= [:out-of-jurisdiction] (distinct (map :violation (:violations %))))
                  (attempts r))))
    (testing "the retry is told what went wrong"
      (is (str/includes? (:stdin (second (attempts r))) "rolled back")))
    (testing "later steps never start"
      (is (not (.exists (io/file (:root env) "test/second")))))))

(deftest a-violation-retry-can-succeed
  (let [env (setup!)
        r (run-it! env {:impl (script-agent (str "if cat | grep -q 'rolled back'; "
                                       "then echo fixed > src/ok.clj; "
                                       "else echo x > deps.edn; fi"))}
                [step-impl])]
    (is (= :success (:status r)))
    (is (= 2 (count (attempts r))))
    (is (not (.exists (io/file (:root env) "deps.edn"))))
    (is (.exists (io/file (:root env) "src/ok.clj")))))

(deftest git-tamper-is-detected-and-restored
  (let [env (setup!)
        head (git/snapshot (:root env))
        r (run-it! env {:impl (script-agent (str "cat >/dev/null; echo x > src/x; unset GIT_DIR; "
                                       "git add src/x; git commit -qm sneaky")
                                  :max-violation-retries 0)}
                [step-impl])
        now (git/snapshot (:root env))]
    (is (= :violations (:status r)))
    (is (some #{:git-tamper} (map :violation (:violations (first (attempts r))))))
    (is (= (:head head) (:head now)))
    (is (= (:index head) (:index now)))
    (is (not (.exists (io/file (:root env) "src/x"))))))

(deftest tampering-with-the-record-cannot-erase-the-runners-entries
  (let [env (setup!)
        r (run-it! env {:impl (script-agent "cat >/dev/null; echo junk > .switchyard/main/runs/zzz.edn"
                                  :max-violation-retries 0)}
                [step-impl])]
    (is (= :violations (:status r)))
    (is (not (.exists (io/file (:dir env) "runs/zzz.edn"))))
    (is (= [:header :attempt :terminal] (map :entry (entries r))))
    (is (= [".switchyard/main/runs/zzz.edn"] (map :path (:violations (first (attempts r))))))))

(deftest expected-output-is-checked
  (testing ":must-change with an empty diff is :no-work"
    (let [r (run-it! (setup!) {:impl (script-agent "cat >/dev/null; true" :must-change true
                                         :max-violation-retries 0)} [step-impl])]
      (is (= :violations (:status r)))
      (is (= [:no-work] (map :violation (:violations (first (attempts r))))))))
  (testing "malformed output is a violation"
    (let [r (run-it! (setup!) {:impl (script-agent "cat >/dev/null; printf '```edn\\n{:a\\n```\\n'"
                                         :max-violation-retries 0)} [step-impl])]
      (is (= [:malformed-output] (map :violation (:violations (first (attempts r))))))))
  (testing ":needs-outside-jurisdiction is carried into the record"
    (let [r (run-it! (setup!)
                  {:impl (script-agent (str "cat >/dev/null; printf '```edn\\n{:needs-outside-jurisdiction "
                                     "[{:path \"deps.edn\" :problem \"p\" :evidence \"e\"}]}\\n```\\n'"))}
                  [step-impl])]
      (is (= :success (:status r)))
      (is (= [{:path "deps.edn" :problem "p" :evidence "e"}]
             (:needs-outside-jurisdiction (first (attempts r))))))))

(deftest lock-handling
  (let [host (.getHostName (InetAddress/getLocalHost))
        agents {:impl (script-agent "cat >/dev/null; true")}
        write-lock! (fn [env data] (spit (io/file (:dir env) "lock") (pr-str data)))]
    (testing "a live holder is a run in progress and the lock is left alone"
      (let [env (setup!)]
        (write-lock! env {:pid (.pid (java.lang.ProcessHandle/current)) :host host})
        (is (= [:run-in-progress] (map :problem (:problems (run-it! env agents [step-impl])))))
        (is (.exists (io/file (:dir env) "lock")))))
    (testing "a dead holder on this host is reported stale, never auto-cleared"
      (let [env (setup!)]
        (write-lock! env {:pid 99999999 :host host})
        (is (= [:stale-lock] (map :problem (:problems (run-it! env agents [step-impl])))))
        (is (.exists (io/file (:dir env) "lock")))
        (testing "--force-unlock clears it and the run proceeds"
          (is (= :success (:status (run-it! env agents [step-impl] {:force-unlock? true})))))))))

(deftest bundled-example-passes-the-runners-own-checks
  (let [dir "resources/timetable"
        timetable (tt/load-dir dir)]
    (is (empty? (pipeline/pipeline-problems
                 {:config {:plugin "opencode"} :timetable timetable
                  :pipeline-name :development :branch "main"
                  :spec {:path "spec.md" :contents "SPEC"}
                  :prompt-texts (sut/read-prompts dir timetable :development)})))))

(deftest exit-precedence
  (testing "violations win over a failing process: exhausted retries end in :violations"
    (let [env (setup!)
          r   (run-it! env {:impl (script-agent "cat >/dev/null; echo x > deps.edn; exit 1"
                                                :max-violation-retries 1)}
                       [step-impl])]
      (is (= :violations (:status r)))
      (is (= [:crashed :crashed] (map :process (attempts r))))
      (is (every? :restored? (attempts r)))
      (is (not (.exists (io/file (:root env) "deps.edn"))))))
  (testing "a crashed attempt with violations is retried, not ended"
    (let [env (setup!)
          r   (run-it! env {:impl (script-agent "cat >/dev/null; echo x > deps.edn; exit 1"
                                                :max-violation-retries 2)}
                       [step-impl])]
      (is (= 3 (count (attempts r))))))
  (testing "a clean crash after a violation is :step-failed"
    (let [env (setup!)
          r   (run-it! env {:impl (script-agent (str "if cat | grep -q 'rolled back'; "
                                                     "then exit 1; else echo x > deps.edn; fi"))}
                       [step-impl])]
      (is (= :step-failed (:status r)))
      (is (= [[:ok [:out-of-jurisdiction]] [:crashed []]]
             (map (juxt :process #(distinct (map :violation (:violations %)))) (attempts r))))))
  (testing "a clean attempt after a violation and a crash succeeds"
    (let [env (setup!)
          r   (run-it! env {:impl (script-agent (str "if cat | grep -q 'rolled back'; "
                                                     "then echo ok > src/ok.clj; "
                                                     "else echo x > deps.edn; exit 1; fi"))}
                       [step-impl])]
      (is (= :success (:status r)))))
  (testing "an agent that cannot be started is a step failure, not a problem"
    (let [env (setup!)
          r   (run-it! env {:impl (script-agent "true" :harness :ghost)} [step-impl])]
      (is (= :step-failed (:status r)))
      (is (= [:harness-error] (map :process (attempts r)))))))

(defn- header [result] (first (entries result)))

(deftest writes-dirs-are-created-ahead-of-time
  (let [env (setup!)
        r   (run-it! env {:impl (script-agent "cat >/dev/null; true"
                                              :writes ["docs/" "gen/"])
                          :other (script-agent "true" :writes ["unused/"])}
                     [step-impl])]
    (is (= :success (:status r)))
    (testing "missing directories are created and listed in the header"
      (is (= ["docs/" "gen/"] (:writes (header r))))
      (is (= ["docs/" "gen/"] (:created-dirs (header r))))
      (is (.isDirectory (io/file (:root env) "docs")))
      (is (.isDirectory (io/file (:root env) "gen"))))
    (testing "an agent the pipeline does not use gets no directory"
      (is (not (.exists (io/file (:root env) "unused")))))
    (testing "existing directories are not reported as created"
      (let [r2 (run-it! env {:impl (script-agent "cat >/dev/null; true" :writes ["docs/"])}
                        [step-impl])]
        (is (= [] (:created-dirs (header r2))))))))

(deftest a-claimed-directory-survives-rollback-and-deletion
  (testing "a rolled-back attempt leaves the claimed directory in place"
    (let [env (setup!)
          r   (run-it! env {:impl (script-agent "cat >/dev/null; echo x > docs/a; echo y > deps.edn"
                                                :writes ["docs/"] :max-violation-retries 0)}
                       [step-impl])]
      (is (= :violations (:status r)))
      (is (.isDirectory (io/file (:root env) "docs")))
      (is (empty? (.list (io/file (:root env) "docs"))))))
  (testing "an agent that removes its own directory gets it back"
    (let [env (setup!)
          r   (run-it! env {:impl (script-agent "cat >/dev/null; rm -rf docs" :writes ["docs/"])}
                       [step-impl])]
      (is (= :success (:status r)))
      (is (.isDirectory (io/file (:root env) "docs")))
      (is (= ["docs/"] (:recreated-dirs (first (attempts r))))))))

(deftest per-directory-trees-are-recorded
  (let [env (setup!)
        r   (run-it! env {:impl (script-agent "cat >/dev/null; echo n > src/new.clj"
                                              :writes ["src/" "docs/"])}
                     [step-impl])
        {:keys [before after]} (:writes-trees (first (attempts r)))]
    (is (= :success (:status r)))
    (testing "a changed directory has different trees before and after"
      (is (string? (get before "src/")))
      (is (not= (get before "src/") (get after "src/"))))
    (testing "an empty directory has no tree"
      (is (nil? (get before "docs/")))
      (is (nil? (get after "docs/"))))))

(deftest a-writes-prefix-that-is-a-file-is-a-problem
  (let [env (setup!)
        _   (gt/put! (:root env) "README.md" "x")
        r   (run-it! env {:impl (script-agent "true" :writes ["README.md/"])} [step-impl])]
    (is (= [:writes-not-directory] (map :problem (:problems r))))
    (is (.isFile (io/file (:root env) "README.md")))))

;; -- Worktree runs ---------------------------------------------------------
;; In a worktree run the agent's cwd is R/.switchyard/worktrees/main-<id>, so
;; ../../main is the runner's state directory and ../../.. is the main checkout.

(defn- wt-run [env agents steps & [extra]]
  (run-it! env agents steps (merge {:worktree? true :run-id "r1"} extra)))

(defn- wt-path [env id] (str (:root env) "/.switchyard/worktrees/main-" id))

(defn- kinds [r] (map :violation (:violations (first (attempts r)))))

(defn- problem-kinds [r] (map :problem (:problems r)))

(deftest worktree-run-isolates-the-main-checkout
  (let [env  (setup!)
        head (str/trim (gt/sh! (:root env) "git" "rev-parse" "HEAD"))
        r    (wt-run env {:impl (script-agent "cat >/dev/null; echo new > src/new.clj")} [step-impl])
        wt   (wt-path env "r1")]
    (is (= :success (:status r)))
    (testing "the agent worked in the worktree; the main checkout is untouched"
      (is (= "new\n" (slurp (io/file wt "src/new.clj"))))
      (is (not (.exists (io/file (:root env) "src/new.clj"))))
      (is (str/blank? (gt/sh! (:root env) "git" "status" "--porcelain"))))
    (testing "it ran on a new branch, and the worktree is reported"
      (is (str/includes? (gt/sh! (:root env) "git" "branch" "--list" "switchyard/main/r1")
                         "switchyard/main/r1"))
      (is (= {:path wt :branch "switchyard/main/r1" :run-id "r1" :reused? false}
             (:worktree r)))
      (is (= (:worktree r) (:worktree (header r)))))
    (testing "nothing is committed or moved"
      (is (= head (str/trim (gt/sh! (:root env) "git" "rev-parse" "HEAD"))))
      (is (= head (str/trim (gt/sh! (:root env) "git" "rev-parse" "switchyard/main/r1")))))
    (testing "setup order: the .gitignore is written before the baselines, so the first attempt is clean"
      (is (= [] (:violations (first (attempts r)))))
      (is (= ["worktrees/" "*/lock" "*/db/"]
             (str/split-lines (slurp (io/file (:root env) ".switchyard/.gitignore"))))))))

(deftest state-gitignore-keeps-existing-lines
  (let [env (setup!)]
    (gt/put! (:root env) ".switchyard/.gitignore" "mine\nworktrees/\n")
    (wt-run env {:impl (script-agent "cat >/dev/null; true")} [step-impl])
    (is (= ["mine" "worktrees/" "*/lock" "*/db/"]
           (str/split-lines (slurp (io/file (:root env) ".switchyard/.gitignore")))))))

(deftest dirty-checkout-is-a-problem-unless-allowed
  (let [agents {:impl (script-agent "cat >/dev/null; true")}]
    (testing "a tracked, uncommitted change stops the run before anything is created"
      (let [env (setup!)]
        (gt/put! (:root env) "src/a.clj" "dirty")
        (let [r (wt-run env agents [step-impl])]
          (is (= [:dirty-checkout] (problem-kinds r)))
          (is (= ["src/a.clj"] (:paths (first (:problems r)))))
          (is (not (.exists (io/file (wt-path env "r1"))))))))
    (testing "--allow-dirty only suppresses the check; agents still see HEAD"
      (let [env (setup!)]
        (gt/put! (:root env) "src/a.clj" "dirty")
        (let [r (wt-run env agents [step-impl] {:allow-dirty? true})]
          (is (= :success (:status r)))
          (is (= "a1" (slurp (io/file (wt-path env "r1") "src/a.clj")))))))
    (testing ".switchyard/ edits and untracked files are not dirty"
      (let [env (setup!)]
        (gt/put! (:root env) ".switchyard/main/p.md" "edited")
        (gt/put! (:root env) "untracked.txt" "x")
        (is (= :success (:status (wt-run env agents [step-impl]))))))))

(deftest run-id-reuses-the-previous-worktree
  (let [env (setup!)
        r1  (wt-run env {:impl (script-agent "cat >/dev/null; echo one >> src/log")} [step-impl])
        r2  (wt-run env {:impl (script-agent "cat >/dev/null; echo two >> src/log")} [step-impl])]
    (is (= :success (:status r1)))
    (is (= :success (:status r2)))
    (is (= "one\ntwo\n" (slurp (io/file (wt-path env "r1") "src/log"))))
    (is (= (:path (:worktree r1)) (:path (:worktree r2))))
    (is (true? (:reused? (:worktree r2))))
    (testing "one record file; the second invocation appends a resumed header"
      (is (= (:record r1) (:record r2)))
      (let [es (entries r2)]
        (is (= [:header :attempt :terminal :header :attempt :terminal] (map :entry es)))
        (is (true? (:resumed? (nth es 3))))
        (is (nil? (:resumed? (first es))))))))

(deftest worktree-setup-problems
  (let [agents {:impl (script-agent "cat >/dev/null; true")}]
    (testing "an unusable run id"
      (is (= [:invalid-run-id] (problem-kinds (wt-run (setup!) agents [step-impl] {:run-id "bad/id"})))))
    (testing "a branch that would collide with the worktrees directory"
      (is (some #{:reserved-branch-name}
                (problem-kinds (wt-run (setup!) agents [step-impl] {:branch "worktrees"})))))
    (testing "a directory that is not a registered worktree is never guessed at"
      (let [env (setup!)]
        (.mkdirs (io/file (wt-path env "r2")))
        (let [r (wt-run env agents [step-impl] {:run-id "r2"})]
          (is (= [:worktree-conflict] (problem-kinds r)))
          (is (= :unregistered-directory (:reason (first (:problems r))))))))
    (testing "a branch without its worktree"
      (let [env (setup!)]
        (gt/sh! (:root env) "git" "branch" "switchyard/main/r3")
        (is (= :branch-without-worktree
               (:reason (first (:problems (wt-run env agents [step-impl] {:run-id "r3"}))))))))
    (testing "a branch named switchyard blocks switchyard/<branch>/<id>, caught before git errors"
      (let [env (setup!)]
        (gt/sh! (:root env) "git" "branch" "switchyard")
        (let [r (wt-run env agents [step-impl])]
          (is (= [:worktree-conflict] (problem-kinds r)))
          (is (= :ref-conflict (:reason (first (:problems r))))))))))

(deftest runner-state-is-protected-and-restored
  (let [env (setup!)
        r   (wt-run env {:impl (script-agent "cat >/dev/null; echo junk > ../../main/runs/zzz.edn"
                                             :max-violation-retries 0)}
                    [step-impl])]
    (is (= :violations (:status r)))
    (is (= [:out-of-jurisdiction] (kinds r)))
    (is (= [".switchyard/main/runs/zzz.edn"] (map :path (:violations (first (attempts r))))))
    (is (not (.exists (io/file (:dir env) "runs/zzz.edn"))))
    (testing "the record survived the rollback"
      (is (= [:header :attempt :terminal] (map :entry (entries r)))))))

(deftest the-main-checkout-is-watched-but-never-restored
  (let [env (setup!)
        r   (wt-run env {:impl (script-agent "cat >/dev/null; echo x > ../../../notes.txt"
                                             :max-violation-retries 2)}
                    [step-impl])]
    (is (= :violations (:status r)))
    (testing "the run stops at once: no retry"
      (is (= 1 (count (attempts r)))))
    (is (= [{:violation :main-checkout-modified :path "notes.txt"}]
           (:violations (first (attempts r)))))
    (testing "nothing in the main checkout is rolled back"
      (is (.exists (io/file (:root env) "notes.txt"))))))

(deftest the-shared-git-directory-is-watched-in-both-modes
  (doseq [[label run script]
          [["worktree" wt-run "cat >/dev/null; echo x > ../../../.git/hooks/pre-commit"]
           ["in place" run-it! "cat >/dev/null; echo x > .git/hooks/pre-commit"]]]
    (testing label
      (let [env (setup!)
            r   (run env {:impl (script-agent script :max-violation-retries 2)} [step-impl])]
        (is (= :violations (:status r)))
        (is (= 1 (count (attempts r))))
        (is (= [{:violation :git-dir-modified :path ".git/hooks/pre-commit"}]
               (:violations (first (attempts r)))))
        (is (.exists (io/file (:root env) ".git/hooks/pre-commit")) "never restored")))))

(deftest reserved-db-state-is-never-snapshotted-or-rolled-back
  (doseq [[label run script]
          [["worktree" wt-run (str "cat >/dev/null; mkdir -p ../../main/db; "
                                   "echo x > ../../main/db/data.mdb; echo y > deps.edn")]
           ["in place" run-it! (str "cat >/dev/null; mkdir -p .switchyard/main/db; "
                                    "echo x > .switchyard/main/db/data.mdb; echo y > deps.edn")]]]
    (testing label
      (let [env (setup!)
            r   (run env {:impl (script-agent script :max-violation-retries 0)} [step-impl])]
        (is (= :violations (:status r)))
        (testing "only the real violation is reported; db/ is invisible"
          (is (= [{:violation :out-of-jurisdiction :path "deps.edn"}]
                 (:violations (first (attempts r))))))
        (testing "the rollback left db/ alone"
          (is (= "x\n" (slurp (io/file (:dir env) "db/data.mdb")))))))))

(deftest an-in-place-run-ignores-earlier-run-worktrees
  (let [env (setup!)]
    (is (= :success (:status (wt-run env {:impl (script-agent "cat >/dev/null; echo n > src/n.clj")}
                                     [step-impl]))))
    (let [r (run-it! env {:impl (script-agent "cat >/dev/null; echo m > src/m.clj")} [step-impl])]
      (is (= :success (:status r)))
      (is (= [] (:violations (first (attempts r))))))))
