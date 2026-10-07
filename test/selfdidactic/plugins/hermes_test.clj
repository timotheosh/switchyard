(ns selfdidactic.plugins.hermes-test
  (:require [clojure.test :refer [deftest is testing]]
            [selfdidactic.plugins.hermes :as sut]))

(def harness
  {:command "hermes"
   :model {:parameter "-m"}
   :skill {:parameter "--skills" :plural true}})

(deftest build-argv-test
  (testing "model and skills map to their CLI flags; the prompt is always --oneshot="
    (doseq [[label agent prompt expected]
            [["multiple skills"
              {:model "anthropic/claude-sonnet-4.6" :skills [:acd :clojure]}
              "do the thing"
              ["hermes" "-m" "anthropic/claude-sonnet-4.6"
               "--skills" "acd" "--skills" "clojure"
               "--oneshot=do the thing"]]
             ["one skill"
              {:model "m" :skills [:acd]}
              "p"
              ["hermes" "-m" "m" "--skills" "acd" "--oneshot=p"]]
             ["empty skills"
              {:model "m" :skills []}
              "p"
              ["hermes" "-m" "m" "--oneshot=p"]]
             ["skills key absent"
              {:model "m"}
              "p"
              ["hermes" "-m" "m" "--oneshot=p"]]
             ["model absent"
              {:skills [:acd]}
              "p"
              ["hermes" "--skills" "acd" "--oneshot=p"]]
             ["namespaced keyword model keeps its namespace"
              {:model :anthropic/claude-sonnet-4.6}
              "p"
              ["hermes" "-m" "anthropic/claude-sonnet-4.6" "--oneshot=p"]]
             ["single-word prompt starting with - is still joined onto --oneshot"
              {:model "m"}
              "-x"
              ["hermes" "-m" "m" "--oneshot=-x"]]]]
      (testing label
        (is (= expected (sut/build-argv harness agent prompt))))))

  (testing "extra params pass through before the prompt"
    (is (= ["hermes" "-m" "m" "--accept-hooks" "--oneshot=p"]
           (sut/build-argv (assoc harness :extra-params ["--accept-hooks"])
                           {:model "m"}
                           "p"))))

  (testing "a nil prompt throws"
    (is (thrown? clojure.lang.ExceptionInfo
                 (sut/build-argv harness {:model "m"} nil))))

  (testing "an empty-string prompt throws"
    (is (thrown? clojure.lang.ExceptionInfo
                 (sut/build-argv harness {:model "m"} ""))))

  (testing "a value with no harness mapping throws"
    (is (thrown? clojure.lang.ExceptionInfo
                 (sut/build-argv (dissoc harness :model) {:model "m"} "p")))))
