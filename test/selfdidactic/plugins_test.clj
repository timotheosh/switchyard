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
