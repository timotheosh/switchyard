(ns selfdidactic.plugins.opencode-test
  (:require [clojure.test :refer [deftest is testing]]
            [selfdidactic.plugins.opencode :as sut]))

(def harness
  {:command "opencode"
   :model {:parameter "-m"}})

(deftest build-command-test
  (testing "model maps to -m and the prompt is delivered on stdin"
    (doseq [[label agent prompt expected]
            [["model and prompt"
              {:model "anthropic/claude-sonnet-4.6"}
              "do the thing"
              {:argv ["opencode" "run" "-m" "anthropic/claude-sonnet-4.6"]
               :stdin "do the thing"}]
             ["model absent"
              {}
              "p"
              {:argv ["opencode" "run"] :stdin "p"}]
             ["empty skills supply nothing"
              {:model "m" :skills []}
              "p"
              {:argv ["opencode" "run" "-m" "m"] :stdin "p"}]
             ["prompt starting with - cannot be mistaken for a flag"
              {:model "m"}
              "-x"
              {:argv ["opencode" "run" "-m" "m"] :stdin "-x"}]]]
      (testing label
        (is (= expected (sut/build-command harness agent prompt))))))

  (testing "extra params pass through"
    (is (= {:argv ["opencode" "run" "-m" "m" "--auto" "--print-logs"] :stdin "p"}
           (sut/build-command (assoc harness :extra-params ["--auto" "--print-logs"])
                              {:model "m"}
                              "p"))))

  (testing "skills are rejected: opencode has no skills flag"
    (is (thrown? clojure.lang.ExceptionInfo
                 (sut/build-command harness {:model "m" :skills [:acd]} "p"))))

  (testing "a nil prompt throws"
    (is (thrown? clojure.lang.ExceptionInfo
                 (sut/build-command harness {:model "m"} nil))))

  (testing "an empty-string prompt throws"
    (is (thrown? clojure.lang.ExceptionInfo
                 (sut/build-command harness {:model "m"} "")))))
