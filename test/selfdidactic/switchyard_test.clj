(ns selfdidactic.switchyard-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [selfdidactic.switchyard :as sut]
            [selfdidactic.version :as version])
  (:import [java.io StringWriter]
           [java.nio.charset StandardCharsets]
           [java.nio.file Files OpenOption]
           [java.nio.file.attribute FileAttribute]))

(def expected-help
  (str "Usage: switchyard [options]\n\n"
       "Options:\n"
       "  -h, --help       Show this help\n"
       "  -v, --version    Show program version\n"
       "  -s, --spec PATH  Path to a Markdown or Org specification document\n"))

(def expected-version "switchyard 0.1.0-SNAPSHOT\n")

(defn run-cli
  [args]
  (let [stdout (StringWriter.)
        stderr (StringWriter.)
        exit-code (binding [*out* stdout
                            *err* stderr]
                    (sut/run-cli! args))]
    {:exit-code exit-code
     :stdout (str stdout)
     :stderr (str stderr)}))

(defn temp-spec
  [suffix contents]
  (let [path (Files/createTempFile "switchyard-test-" suffix
                                   (make-array FileAttribute 0))]
    (Files/writeString path contents StandardCharsets/UTF_8
                       (make-array OpenOption 0))
    path))

(defn assert-usage-error
  [args reason]
  (let [{:keys [exit-code stdout stderr]} (run-cli args)]
    (is (= 2 exit-code))
    (is (= "" stdout))
    (is (str/includes? stderr reason))
    (is (str/includes? stderr expected-help))))

(defn assert-specification-error
  [path reason]
  (let [{:keys [exit-code stdout stderr]} (run-cli ["--spec" path])]
    (is (= 1 exit-code))
    (is (= "" stdout))
    (is (str/includes? stderr path))
    (is (str/includes? stderr reason))
    (is (not (str/includes? stderr "Usage:")))
    (is (not (str/includes? stderr "Exception")))))

(deftest program-version-is-shared
  (is (= "0.1.0-SNAPSHOT" version/program-version)))

(deftest help-and-version-output
  (doseq [args [[] ["-h"] ["--help"]]]
    (testing (str "help arguments " (pr-str args))
      (is (= {:exit-code 0
              :stdout expected-help
              :stderr ""}
             (run-cli args)))))
  (doseq [args [["-v"] ["--version"]]]
    (testing (str "version arguments " (pr-str args))
      (is (= {:exit-code 0
              :stdout expected-version
              :stderr ""}
             (run-cli args))))))

(deftest informational-options-take-precedence
  (with-redefs [sut/load-specification!
                (fn [_]
                  (throw (AssertionError. "specification should not be read")))]
    (doseq [args [["--version" "--help"]
                  ["--unknown" "--help"]
                  ["--spec" "--help"]
                  ["--help" "--spec" "/definitely/missing.md"]
                  ["--help=garbage" "--help"]
                  ["-hv"]]]
      (testing (str "help wins for " (pr-str args))
        (is (= {:exit-code 0
                :stdout expected-help
                :stderr ""}
               (run-cli args)))))
    (doseq [args [["--unknown" "--version"]
                  ["--spec" "--version"]
                  ["--version" "--spec" "/definitely/missing.md"]
                  ["--help=garbage" "--version"]]]
      (testing (str "version wins for " (pr-str args))
        (is (= {:exit-code 0
                :stdout expected-version
                :stderr ""}
               (run-cli args)))))))

(deftest parses-short-and-long-spec-options
  (is (= {:kind :spec :path "example.md"}
         (sut/parse-command ["-s" "example.md"])))
  (is (= {:kind :spec :path "example.org"}
         (sut/parse-command ["--spec" "example.org"])))
  (is (= {:kind :spec :path "example.markdown"}
         (sut/parse-command ["--spec=example.markdown"]))))

(deftest reports-command-line-errors
  (assert-usage-error ["--wat"] "Unknown option: \"--wat\"")
  (assert-usage-error ["--spec"]
                      "Missing required argument for \"--spec PATH\"")
  (assert-usage-error ["-s"]
                      "Missing required argument for \"-s PATH\"")
  (assert-usage-error ["--spec="]
                      "Option --spec requires a non-blank path.")
  (assert-usage-error ["-s" "   "]
                      "Option --spec requires a non-blank path.")
  (assert-usage-error ["--help=garbage"]
                      "Option --help does not accept a value.")
  (assert-usage-error ["--version=garbage"]
                      "Option --version does not accept a value.")
  (assert-usage-error ["doc.md"] "Unexpected argument(s): doc.md")
  (assert-usage-error ["-s" "a.md" "extra"]
                      "Unexpected argument(s): extra")
  (assert-usage-error ["--"] "Option --spec is required.")
  (assert-usage-error ["--" "--help"]
                      "Unexpected argument(s): --help")
  (assert-usage-error ["-s" "/missing-a.md"
                       "--spec" "/missing-b.org"]
                      "Option --spec may be specified only once.")
  (assert-usage-error ["-s" "/same.md" "-s" "/same.md"]
                      "Option --spec may be specified only once."))

(deftest recognizes-supported-formats
  (doseq [[path format] [["spec.md" :markdown]
                         ["spec.markdown" :markdown]
                         ["spec.org" :org]
                         ["SPEC.MD" :markdown]
                         ["SPEC.MARKDOWN" :markdown]
                         ["SPEC.ORG" :org]]]
    (is (= format (sut/specification-format path))))
  (is (nil? (sut/specification-format "spec.txt"))))

(deftest loads-supported-specifications-and-succeeds-silently
  (let [contents "# Héllø λ\nSecond line\n"]
    (doseq [[suffix format] [[".md" :markdown]
                             [".markdown" :markdown]
                             [".org" :org]
                             [".MD" :markdown]
                             [".ORG" :org]]]
      (let [path (temp-spec suffix contents)
            path-string (str path)]
        (try
          (is (= {:path path-string
                  :format format
                  :contents contents}
                 (sut/load-specification! path-string)))
          (is (= {:exit-code 0
                  :stdout ""
                  :stderr ""}
                 (run-cli ["--spec" path-string])))
          (finally
            (Files/deleteIfExists path)))))))

(deftest empty-specification-is-valid
  (let [path (temp-spec ".md" "")]
    (try
      (is (= "" (:contents (sut/load-specification! (str path)))))
      (is (= {:exit-code 0 :stdout "" :stderr ""}
             (run-cli ["-s" (str path)])))
      (finally
        (Files/deleteIfExists path)))))

(deftest reports-specification-errors
  (testing "unsupported extension"
    (let [path (temp-spec ".txt" "plain text")]
      (try
        (assert-specification-error (str path) "unsupported format")
        (finally
          (Files/deleteIfExists path)))))
  (testing "missing file"
    (let [path (temp-spec ".md" "soon deleted")]
      (Files/deleteIfExists path)
      (try
        (assert-specification-error (str path)
                                    "path is not a readable regular file")
        (finally
          (Files/deleteIfExists path)))))
  (testing "directory instead of a regular file"
    (let [root (Files/createTempDirectory "switchyard-test-"
                                          (make-array FileAttribute 0))
          directory (Files/createDirectory (.resolve root "spec.org")
                                           (make-array FileAttribute 0))]
      (try
        (assert-specification-error (str directory)
                                    "path is not a readable regular file")
        (finally
          (Files/deleteIfExists directory)
          (Files/deleteIfExists root))))))
