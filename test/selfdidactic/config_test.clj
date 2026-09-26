(ns selfdidactic.config-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [selfdidactic.config :as sut])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- temp-toml [contents]
  (let [path (Files/createTempFile "switchyard-test-" ".toml"
                                   (make-array FileAttribute 0))]
    (spit (.toFile path) contents)
    (str path)))

(deftest toml->edn-returns-a-keyword-map
  (is (= {:spec "a.md" :section {:n 1}}
         (sut/toml->edn "spec = \"a.md\"\n[section]\nn = 1")))
  (is (= {} (sut/toml->edn ""))))

(deftest merge-overrides-cli-wins
  (is (= {:spec "cli.md" :other 1}
         (sut/merge-overrides {:spec "file.md" :other 1} {:spec "cli.md"}))))

(deftest show-str-is-edn
  (is (= {:spec "a.md"} (read-string (sut/show-str {:spec "a.md"})))))

(def two-keys {:spec {:flag "--spec" :toml "spec"}
               :out {:flag "--out" :toml "out"}})

(deftest missing-keys-and-messages
  (is (= [] (sut/missing-keys two-keys {:spec 1 :out 2})))
  (is (= [:out] (sut/missing-keys two-keys {:spec 1})))
  (let [missing (sut/missing-keys two-keys {})
        message (sut/describe-missing two-keys missing)]
    (is (= 2 (count missing)))
    (is (str/includes? message "spec: --spec or `spec` in the config file"))
    (is (str/includes? message "out: --out or `out` in the config file"))))

(deftest key-sources-name-the-origin
  (is (= {:spec "--spec" :out "cfg.toml"}
         (sut/key-sources {:spec "cli.md"} {:spec "f.md" :out "o"} "cfg.toml"))))

(deftest load-config-reads-only-the-files-keys
  (let [path (temp-toml "spec = \"x.md\"\n")]
    (is (= {:spec "x.md"} (sut/load-config! path)))))

(deftest load-config-ignores-system-properties
  (System/setProperty "spec" "from-sysprop")
  (try
    (is (= {:spec "x.md"} (sut/load-config! (temp-toml "spec = \"x.md\"\n"))))
    (is (= {} (sut/load-config! (temp-toml ""))))
    (finally (System/clearProperty "spec"))))

(deftest load-config-sources
  (testing "default resource is the bundled file"
    (is (contains? (sut/load-config! nil) :spec)))
  (testing "missing default resource is an empty config"
    (is (= {} (sut/load-config! nil "no-such-resource.toml"))))
  (testing "missing explicit file is an error"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not found"
                          (sut/load-config! "/definitely/missing.toml"))))
  (testing "invalid TOML is an error"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"invalid TOML"
                          (sut/load-config! (temp-toml "spec = = =\n"))))))
