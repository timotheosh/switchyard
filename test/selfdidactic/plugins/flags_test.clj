(ns selfdidactic.plugins.flags-test
  (:require [clojure.test :refer [deftest is testing]]
            [selfdidactic.plugins.flags :as sut]))

(deftest flag-args-test
  (testing "supplies nothing for nil or empty-collection values"
    (doseq [[mapping value] [[{:parameter "-m"} nil]
                             [{:parameter "--skills" :plural true} []]
                             [{:parameter "--skills" :plural true} nil]
                             [nil nil]
                             [nil []]]]
      (is (= [] (sut/flag-args mapping value)))))

  (testing "single value is one flag/value pair"
    (is (= ["-m" "gpt"] (sut/flag-args {:parameter "-m"} "gpt"))))

  (testing "plural repeats the flag per value"
    (is (= ["--skills" "acd" "--skills" "clojure"]
           (sut/flag-args {:parameter "--skills" :plural true}
                          [:acd :clojure]))))

  (testing "keyword values keep their namespace"
    (is (= ["-m" "anthropic/claude-sonnet-4.6"]
           (sut/flag-args {:parameter "-m"} :anthropic/claude-sonnet-4.6)))
    (is (= ["--skills" "team/acd"]
           (sut/flag-args {:parameter "--skills" :plural true} [:team/acd]))))

  (testing "non-string values are stringified"
    (is (= ["--n" "3"] (sut/flag-args {:parameter "--n"} 3))))

  (testing "a supplied value with no mapping throws"
    (is (thrown? clojure.lang.ExceptionInfo (sut/flag-args nil "x")))
    (is (thrown? clojure.lang.ExceptionInfo (sut/flag-args {} ["a"])))))
