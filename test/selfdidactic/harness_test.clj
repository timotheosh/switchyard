(ns selfdidactic.harness-test
  (:require [clojure.java.shell :as shell]
            [clojure.test :refer [deftest is testing]]
            [selfdidactic.harness :as sut]))

(deftest classify-test
  (doseq [[label result expected]
          [["success"
            {:exit 0 :out "answer" :err ""}
            {:status :success :exit-code 0 :out "answer" :err ""}]
           ["no output"
            {:exit 1 :out "" :err ""}
            {:status :error :exit-code 1 :out "" :err "" :reason :no-output}]
           ["failed"
            {:exit 2 :out "" :err "boom"}
            {:status :error :exit-code 2 :out "" :err "boom" :reason :failed}]
           ["interrupted"
            {:exit 130 :out "" :err ""}
            {:status :error :exit-code 130 :out "" :err "" :reason :interrupted}]
           ["unrecognized exit code"
            {:exit 137 :out "" :err ""}
            {:status :error :exit-code 137 :out "" :err "" :reason :unknown}]]]
    (testing label
      (is (= expected (sut/classify result))))))

(deftest start-test
  (testing "runs argv in dir and classifies the result"
    (let [captured (atom nil)]
      (with-redefs [shell/sh (fn [& args]
                               (reset! captured args)
                               {:exit 0 :out "ok" :err ""})]
        (is (= {:status :success :exit-code 0 :out "ok" :err ""}
               (sut/start ["hermes" "-m" "m"] "/work/dir")))
        (is (= ["hermes" "-m" "m" :dir "/work/dir"] @captured)))))

  (testing "a non-zero exit is classified as an error"
    (with-redefs [shell/sh (constantly {:exit 2 :out "" :err "bad flag"})]
      (is (= {:status :error :exit-code 2 :out "" :err "bad flag" :reason :failed}
             (sut/start ["hermes"] "/work/dir")))))

  (testing "a nil dir throws"
    (is (thrown? clojure.lang.ExceptionInfo
                 (sut/start ["hermes"] nil)))))
