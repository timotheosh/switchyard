(ns selfdidactic.plugins.kiro-test
  (:require [clojure.test :refer [deftest is testing]]
            [selfdidactic.plugins.kiro :as sut]))

(def harness
  {:command "kiro-cli"
   :model {:parameter "--model"}})

(def base ["kiro-cli" "chat" "--no-interactive"])

(deftest build-command-test
  (testing "model maps to --model and the prompt is delivered on stdin"
    (doseq [[label agent prompt expected]
            [["model and prompt"
              {:model "claude-sonnet-5.5"}
              "do the thing"
              {:argv (into base ["--model" "claude-sonnet-5.5"]) :stdin "do the thing"}]
             ["model absent"
              {}
              "p"
              {:argv base :stdin "p"}]
             ["empty skills supply nothing"
              {:model "m" :skills []}
              "p"
              {:argv (into base ["--model" "m"]) :stdin "p"}]
             ["prompt starting with - cannot be mistaken for a flag"
              {:model "m"}
              "-x"
              {:argv (into base ["--model" "m"]) :stdin "-x"}]]]
      (testing label
        (is (= expected (sut/build-command harness agent prompt))))))

  (testing "extra params pass through"
    (is (= {:argv (into base ["--model" "m" "--trust-tools=fs_read,fs_write"]) :stdin "p"}
           (sut/build-command
            (assoc harness :extra-params ["--trust-tools=fs_read,fs_write"])
            {:model "m"} "p"))))

  (testing "--agent in extra params lands after chat --no-interactive and the model flag"
    (is (= {:argv (into base ["--model" "m" "--agent" "reviewer"]) :stdin "p"}
           (sut/build-command
            (assoc harness :extra-params ["--agent" "reviewer"])
            {:model "m"} "p"))))

  (testing "skills are rejected: kiro-cli has no skills flag"
    (is (thrown? clojure.lang.ExceptionInfo
                 (sut/build-command harness {:model "m" :skills [:acd]} "p"))))

  (testing "a nil prompt throws"
    (is (thrown? clojure.lang.ExceptionInfo
                 (sut/build-command harness {:model "m"} nil))))

  (testing "an empty-string prompt throws"
    (is (thrown? clojure.lang.ExceptionInfo
                 (sut/build-command harness {:model "m"} "")))))

(deftest exit-reasons-test
  (is (= {1 :error 3 :mcp-startup-failed 4 :agent-not-found 143 :interrupted}
         sut/exit-reasons)))
