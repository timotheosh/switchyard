(ns selfdidactic.plugins.opencode-test
  (:require [clojure.test :refer [deftest is testing]]
            [selfdidactic.plugins.opencode :as sut]))

(def harness
  {:command "opencode"
   :model {:parameter "-m"}})

(deftest build-argv-test
  (testing "model maps to -m and the prompt follows --"
    (doseq [[label agent prompt expected]
            [["model and prompt"
              {:model "anthropic/claude-sonnet-4.6"}
              "do the thing"
              ["opencode" "run" "-m" "anthropic/claude-sonnet-4.6"
               "--" "do the thing"]]
             ["model absent"
              {}
              "p"
              ["opencode" "run" "--" "p"]]
             ["empty skills supply nothing"
              {:model "m" :skills []}
              "p"
              ["opencode" "run" "-m" "m" "--" "p"]]
             ["prompt starting with - stays a message"
              {:model "m"}
              "-x"
              ["opencode" "run" "-m" "m" "--" "-x"]]]]
      (testing label
        (is (= expected (sut/build-argv harness agent prompt))))))

  (testing "skills are rejected: opencode has no skills flag"
    (is (thrown? clojure.lang.ExceptionInfo
                 (sut/build-argv harness {:model "m" :skills [:acd]} "p"))))

  (testing "a nil prompt throws"
    (is (thrown? clojure.lang.ExceptionInfo
                 (sut/build-argv harness {:model "m"} nil))))

  (testing "an empty-string prompt throws"
    (is (thrown? clojure.lang.ExceptionInfo
                 (sut/build-argv harness {:model "m"} "")))))
