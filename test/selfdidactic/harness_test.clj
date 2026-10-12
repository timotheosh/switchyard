(ns selfdidactic.harness-test
  (:require [clojure.test :refer [deftest is testing]]
            [selfdidactic.harness :as sut])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(deftest classify-test
  (doseq [[label result expected-process]
          [["success"                    {:exit 0 :out "" :err ""} :ok]
           ["opencode error exit"        {:exit 1 :out "" :err ""} :crashed]
           ["SIGTERM (opencode: 143)"    {:exit 143 :out "" :err ""} :crashed]
           ["SIGKILL"                    {:exit 137 :out "" :err ""} :crashed]
           ["timed out wins over exit"   {:exit 143 :out "" :err "" :timed-out? true} :timeout]]]
    (testing label
      (is (= expected-process (:process (sut/classify result)))))))

(defn- temp-dir []
  (str (Files/createTempDirectory "switchyard-harness-" (make-array FileAttribute 0))))

(defn- sh [script & [options]]
  (sut/start ["sh" "-c" script] (temp-dir) (or options {})))

(deftest start-test
  (testing "captures output and exit code in dir"
    (let [dir (temp-dir)]
      (is (= {:process :ok :exit-code 0 :out (str dir "\n") :err ""}
             (sut/start ["sh" "-c" "pwd"] dir)))))

  (testing "a non-zero exit is :crashed with its exit code"
    (is (= {:process :crashed :exit-code 2 :out "" :err "bad\n"}
           (sh "echo bad >&2; exit 2"))))

  (testing "stdin and env reach the child"
    (is (= "hello VAL\n"
           (:out (sh "printf '%s ' \"$(cat)\"; echo \"$V\"" {:stdin "hello" :env {"V" "VAL"}})))))

  (testing "a missing binary is a :harness-error"
    (is (= :harness-error (:process (sut/start ["/definitely/not/here"] (temp-dir))))))

  (testing "a nil dir throws"
    (is (thrown? clojure.lang.ExceptionInfo (sut/start ["true"] nil))))

  (testing "a chatty child filling a pipe is not a false timeout"
    (let [r (sh "head -c 1000000 /dev/zero | tr '\\0' x" {:timeout-ms 20000})]
      (is (= :ok (:process r)))
      (is (= 1000000 (count (:out r))))))

  (testing "timeout kills the process and its descendants"
    (let [dir (temp-dir)
          marker (str dir "/marker")
          r (sut/start ["sh" "-c" (str "(while true; do echo x >> " marker "; sleep 0.05; done) & wait")]
                       dir {:timeout-ms 700})
          size #(.length (java.io.File. marker))
          before (size)]
      (is (= :timeout (:process r)))
      (Thread/sleep 400)
      (is (= before (size)) "the child stopped writing once start returned"))))
