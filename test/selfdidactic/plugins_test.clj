(ns selfdidactic.plugins-test
  (:require [clojure.test :refer [deftest is testing]]
            [selfdidactic.config :as config]
            [selfdidactic.plugins :as sut]))

(def harness {:command "opencode" :model {:parameter "-m"}})

(deftest build-command-uses-the-configured-plugin
  (testing "the bundled config defaults to opencode"
    (is (= {:argv ["opencode" "run" "-m" "m"] :stdin "p"}
           (sut/build-command! (config/load-config! nil) harness {:model "m"} "p"))))
  (testing "the plugin key selects the plugin"
    (is (= {:argv ["opencode" "-m" "m" "--oneshot=p"]}
           (sut/build-command! {:plugin "hermes"} harness {:model "m"} "p"))))
  (testing "a missing or unknown plugin fails loudly"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"No plugin configured"
                          (sut/build-command! {} harness {} "p")))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Unknown plugin: nope"
                          (sut/build-command! {:plugin "nope"} harness {} "p")))))

(deftest exit-reasons-test
  (testing "each plugin supplies its harness's exit-code names"
    (is (= {1 :no-output 2 :failed 130 :interrupted}
           (sut/exit-reasons {:plugin "hermes"} {})))
    (is (= {1 :error 130 :interrupted 137 :killed 143 :terminated}
           (sut/exit-reasons {:plugin "opencode"} {}))))
  (testing "the harness's :exit-codes extend and override the plugin's"
    (is (= {1 :auth 130 :interrupted 137 :killed 143 :terminated 9 :oom}
           (sut/exit-reasons {:plugin "opencode"} {:exit-codes {1 :auth 9 :oom}}))))
  (testing "a plugin without exit-reasons has none"
    (is (= {} (sut/exit-reasons {:plugin "flags"} {})))
    (is (= {7 :x} (sut/exit-reasons {:plugin "flags"} {:exit-codes {7 :x}}))))
  (testing "no plugin configured fails loudly"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"No plugin configured"
                          (sut/exit-reasons {} {})))))
