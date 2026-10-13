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
           :timetable {:harnesses {:fake {:command "sh"}}
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
