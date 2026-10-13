(ns selfdidactic.pipeline-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [selfdidactic.pipeline :as sut]))

(def timetable
  {:harnesses {:fake {:command "sh"}
               :oc   {:command "opencode" :model {:parameter "-m"}}}
   :agents    {:tw   {:harness :fake :model "true" :writes ["test/"]}
               :impl {:harness :fake :model "true" :writes ["src/"]}}
   :prompts   {:p {:path "p.md"}}
   :pipelines {:dev {:steps [{:agent :tw :prompt :p}
                             {:agent :impl :prompt :p}]}}})

(def ctx
  {:config {:plugin "fake"} :timetable timetable :pipeline-name :dev
   :branch "main" :spec {:path "s.md" :contents "SPEC"}
   :prompt-texts {:p {:text "PROMPT"}}})

(defn- problem-kinds [c] (set (map :problem (sut/pipeline-problems c))))

(deftest branch-directory-is-injective
  (is (not= (sut/encode-branch "feat/a-b") (sut/encode-branch "feat-a/b")))
  (is (not= (sut/encode-branch "a%2Fb") (sut/encode-branch "a/b")))
  (is (= "feat%2Fx" (sut/encode-branch "feat/x")))
  (is (= "/r/.switchyard/feat%2Fx" (sut/timetable-dir "/r" "feat/x"))))

(deftest compose-prompt-test
  (is (= "PROMPT\n\n## Specification (s.md)\n\nSPEC"
         (sut/compose-prompt {:prompt-text "PROMPT" :spec {:path "s.md" :contents "SPEC"}
                              :context {}})))
  (testing "violations in the context are rendered for the agent"
    (let [text (sut/compose-prompt
                {:prompt-text "P" :spec {:path "s" :contents "S"}
                 :context {:violations [{:violation :out-of-jurisdiction :path "deps.edn"}]
                           :writes ["src/"]}})]
      (is (str/includes? text "rolled back"))
      (is (str/includes? text "deps.edn"))
      (is (str/includes? text "\"src/\""))
      (is (str/includes? text ":needs-outside-jurisdiction")))))

(deftest outcomes-test
  (testing "only a clean process and no violations continue the run"
    (is (sut/step-continues? {:process :ok :violations []}))
    (is (not (sut/step-continues? {:process :crashed :violations []})))
    (is (not (sut/step-continues? {:process :timeout :violations []})))
    (is (not (sut/step-continues? {:process :ok :violations [{:violation :no-work}]}))))
  (testing "a verdict is data, never a failure"
    (doseq [v [:none :approve :request-changes :refuse]]
      (is (sut/step-continues? {:process :ok :verdict v :violations []})))))

(deftest jurisdiction-violations-test
  (is (= [{:violation :out-of-jurisdiction :path "deps.edn"}
          {:violation :out-of-jurisdiction :path ".switchyard/x/lock"}]
         (sut/jurisdiction-violations ["src/" "test/"]
                                      ["src/a.clj" "deps.edn" "test/t.clj" ".switchyard/x/lock"])))
  (testing "a prefix is a directory, not a name prefix"
    (is (= 1 (count (sut/jurisdiction-violations ["src/"] ["src2/a.clj"]))))))

(deftest agent-output-test
  (let [fence (fn [body] (str "chatter\n```edn\n" body "\n```\ntrailing"))
        finding "{:problem \"p\" :evidence \"e\" :justification \"j\" :required-outcome \"o\"}"]
    (testing "no block is empty structured output"
      (is (= {:output {}} (sut/parse-agent-output "just prose"))))
    (testing "the last block wins"
      (is (= {:output {:b 2}}
             (sut/parse-agent-output (str (fence "{:a 1}") (fence "{:b 2}"))))))
    (testing "unreadable or non-map blocks are malformed"
      (is (:malformed (sut/parse-agent-output (fence "{:a"))))
      (is (:malformed (sut/parse-agent-output (fence "[1 2]")))))
    (testing "malformed output is a violation"
      (is (= [:malformed-output]
             (map :violation (sut/output-violations {} (sut/parse-agent-output (fence "{:a")))))))
    (testing "a findings agent must supply findings"
      (is (= [:malformed-output]
             (map :violation (sut/output-violations {:expects :findings}
                                                    (sut/parse-agent-output "prose")))))
      (is (empty? (sut/output-violations
                   {:expects :findings}
                   (sut/parse-agent-output (fence (str "{:findings [" finding "]}")))))))
    (testing "needs-outside-jurisdiction is validated"
      (is (empty? (sut/output-violations
                   {} {:output {:needs-outside-jurisdiction
                                [{:path "deps.edn" :problem "p" :evidence "e"}]}})))
      (is (seq (sut/output-violations
                {} {:output {:needs-outside-jurisdiction [{:path "x"}]}}))))))

(deftest step-violations-test
  (let [snap {:head "h" :head-ref "r" :refs {} :index "i"}
        agent {:writes ["src/"] :must-change true}]
    (is (= [:no-work] (map :violation (sut/step-violations agent snap snap [] {:output {}}))))
    (is (= [:git-tamper] (map :violation (sut/step-violations
                                          agent snap (assoc snap :head "h2") ["src/a"] {:output {}}))))
    (is (empty? (sut/step-violations agent snap snap ["src/a"] {:output {}})))))

(deftest overlapping-writes-test
  (is (= [{:problem :overlapping-writes :agents [:a :b] :prefixes ["src/" "src/deep/"]}]
         (sut/overlapping-writes {:a {:writes ["src/"]} :b {:writes ["src/deep/"]}})))
  (is (= 1 (count (sut/overlapping-writes {:a {:writes ["src/"]} :b {:writes ["src/"]}}))))
  (is (empty? (sut/overlapping-writes {:a {:writes ["src/"]} :b {:writes ["test/"]}}))))

(deftest pipeline-problems-test
  (testing "a sound pipeline has no problems"
    (is (empty? (sut/pipeline-problems ctx))))
  (testing "detached HEAD"
    (is (contains? (problem-kinds (assoc ctx :branch nil)) :detached-head)))
  (testing "unknown pipeline"
    (is (= #{:unknown-pipeline} (problem-kinds (assoc ctx :pipeline-name :nope)))))
  (testing "agent without :writes"
    (is (contains? (problem-kinds (update-in ctx [:timetable :agents :tw] dissoc :writes))
                   :missing-writes)))
  (testing "bad and overlapping :writes"
    (is (contains? (problem-kinds (assoc-in ctx [:timetable :agents :tw :writes] ["test"]))
                   :bad-writes-prefix))
    (is (contains? (problem-kinds (assoc-in ctx [:timetable :agents :tw :writes] [".switchyard/"]))
                   :bad-writes-prefix))
    (is (contains? (problem-kinds (assoc-in ctx [:timetable :agents :tw :writes] ["src/x/"]))
                   :overlapping-writes)))
  (testing "references and unsupported step keys"
    (let [steps (fn [s] (assoc-in ctx [:timetable :pipelines :dev :steps] s))]
      (is (contains? (problem-kinds (steps [{:agent :ghost :prompt :p}])) :unknown-agent))
      (is (contains? (problem-kinds (steps [{:agent :tw :prompt :ghost}])) :unknown-prompt))
      (is (contains? (problem-kinds (steps [{:agent :tw}])) :missing-prompt))
      (is (contains? (problem-kinds (steps [{:agent :tw :prompt :p :callback :tw}]))
                     :unsupported-step-key))
      (is (contains? (problem-kinds (steps [{:agent :concurrent :thread []}]))
                     :unsupported-step-key))))
  (testing "an unreadable prompt file"
    (is (contains? (problem-kinds (assoc ctx :prompt-texts {:p {:error "nope"}}))
                   :unreadable-prompt)))
  (testing "a skills/plugin mismatch is data, not an exception"
    (let [c (-> ctx
                (assoc :config {:plugin "opencode"})
                (assoc-in [:timetable :agents :tw]
                          {:harness :oc :model "m" :writes ["test/"] :skills [:acd]})
                (assoc-in [:timetable :agents :impl]
                          {:harness :oc :model "m" :writes ["src/"]}))]
      (is (= #{:command} (problem-kinds c)))
      (is (empty? (sut/pipeline-problems (assoc-in c [:timetable :agents :tw :skills] []))))))
  (testing "an unknown plugin is data too"
    (is (contains? (problem-kinds (assoc ctx :config {:plugin "nope"})) :command)))
  (testing "an argv-delivered prompt over the limit"
    (is (empty? (sut/pipeline-problems
                 (assoc ctx :spec {:path "s" :contents (apply str (repeat 300000 "x"))}))))
    (is (contains? (problem-kinds
                    (-> ctx
                        (assoc :config {:plugin "hermes"})
                        (assoc-in [:timetable :agents :tw :harness] :oc)
                        (assoc-in [:timetable :agents :impl :harness] :oc)
                        (assoc :spec {:path "s" :contents (apply str (repeat 300000 "x"))})))
                   :argv-too-long))))

(deftest lock-problem-test
  (is (= :run-in-progress
         (:problem (sut/lock-problem {:pid 1 :host "h"} "h" true))))
  (is (= :run-in-progress
         (:problem (sut/lock-problem {:pid 1 :host "other"} "h" false))))
  (is (= :stale-lock
         (:problem (sut/lock-problem {:pid 1 :host "h"} "h" false))))
  (is (= :run-in-progress (:problem (sut/lock-problem 'pid "h" false)))))

(deftest writes-list-test
  (testing "the sorted, distinct prefixes of the agents the pipeline uses"
    (is (= ["src/" "test/"] (sut/writes-list timetable :dev))))
  (testing "an agent the pipeline does not use contributes nothing"
    (is (= ["src/" "test/"]
           (sut/writes-list (assoc-in timetable [:agents :unused]
                                      {:harness :fake :model "m" :writes ["unused/"]})
                            :dev))))
  (testing "empty :writes, repeats and a missing key contribute nothing"
    (let [t (-> timetable
                (assoc-in [:agents :ro] {:harness :fake :model "m" :writes []})
                (assoc-in [:agents :bare] {:harness :fake :model "m"})
                (assoc-in [:pipelines :dev :steps]
                          [{:agent :ro :prompt :p} {:agent :bare :prompt :p}
                           {:agent :impl :prompt :p} {:agent :impl :prompt :p}]))]
      (is (= ["src/"] (sut/writes-list t :dev)))))
  (testing "an unknown pipeline has none"
    (is (= [] (sut/writes-list timetable :nope)))))

(deftest writes-not-directory-test
  (is (empty? (sut/pipeline-problems (assoc ctx :writes-fs {"src/" :dir "test/" :missing}))))
  (is (= #{:writes-not-directory}
         (problem-kinds (assoc ctx :writes-fs {"src/" :file "test/" :dir})))))

(deftest run-id-and-worktree-naming
  (is (every? sut/valid-run-id? ["r1" "20261013T170516Z-ab12cd" "a.b_c-d"]))
  (is (not-any? sut/valid-run-id? [nil "" "a/b" "../x" ".hidden" "-x" "a b" "r1\n"]))
  (is (= "feat%2Fx-r1" (sut/worktree-name "feat%2Fx" "r1")))
  (is (= "switchyard/feat%2Fx/r1" (sut/worktree-branch "feat%2Fx" "r1"))))

(deftest worktree-problems-test
  (let [base {:worktree? true :branch "main" :run-id "r1" :worktree-facts {}}
        kinds #(set (map (juxt :problem :reason) (sut/worktree-problems %)))]
    (is (empty? (sut/worktree-problems base)))
    (testing "nothing is checked when running in place"
      (is (empty? (sut/worktree-problems (assoc base :worktree? false :run-id "bad/id" :dirty-paths ["x"])))))
    (testing "a dirty checkout, unless allowed"
      (is (= #{[:dirty-checkout nil]} (kinds (assoc base :dirty-paths ["a"]))))
      (is (empty? (sut/worktree-problems (assoc base :dirty-paths ["a"] :allow-dirty? true)))))
    (testing "a registered worktree is re-used: no conflict, no dirty check"
      (is (empty? (sut/worktree-problems
                   (assoc base :dirty-paths ["a"]
                          :worktree-facts {:registered? true :dir-exists? true :branch-exists? true})))))
    (testing "conflicts are named, never guessed"
      (is (= #{[:worktree-conflict :unregistered-directory]}
             (kinds (assoc base :worktree-facts {:dir-exists? true}))))
      (is (= #{[:worktree-conflict :branch-without-worktree]}
             (kinds (assoc base :worktree-facts {:branch-exists? true}))))
      (is (= #{[:worktree-conflict :ref-conflict]}
             (kinds (assoc base :worktree-facts {:ref-conflicts ["switchyard"]})))))
    (testing "a branch named like the container directory, and a bad run id"
      (is (contains? (kinds (assoc base :branch "worktrees")) [:reserved-branch-name nil]))
      (is (contains? (kinds (assoc base :run-id "a/b")) [:invalid-run-id nil])))))

(deftest watcher-violations-test
  (testing "runner state is out of jurisdiction; the rest stops the run"
    (is (= [{:violation :out-of-jurisdiction :path ".switchyard/main/runs/x"}
            {:violation :main-checkout-modified :path "notes.txt"}
            {:violation :git-dir-modified :path ".git/hooks/pre-commit"}]
           (sut/watcher-violations
            {:state-changed [".switchyard/main/runs/x"] :main-changed ["notes.txt"]
             :git-dir-before {"config" "h"} :git-dir-after {"config" "h" "hooks/pre-commit" "n"}}))))
  (testing "nothing changed, nothing reported"
    (is (empty? (sut/watcher-violations {:git-dir-before {"config" "h"} :git-dir-after {"config" "h"}}))))
  (testing "only the two outside-the-worktree kinds stop a run"
    (is (every? sut/stop-violation? [{:violation :main-checkout-modified} {:violation :git-dir-modified}]))
    (is (not-any? sut/stop-violation? [{:violation :out-of-jurisdiction} {:violation :git-tamper}
                                       {:violation :no-work} {:violation :malformed-output}]))))
