(ns selfdidactic.git-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [selfdidactic.git :as sut]
            [selfdidactic.harness :as harness])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn sh! [dir & argv]
  (let [r (harness/start (vec argv) dir)]
    (assert (= :ok (:process r)) (pr-str argv r))
    (:out r)))

(defn put! [dir path content]
  (let [f (io/file dir path)]
    (io/make-parents f)
    (spit f content)))

(defn make-repo
  "A repo with one commit: src/a.clj, test/t.clj, an ignored .switchyard/."
  []
  (let [dir (str (Files/createTempDirectory "switchyard-git-" (make-array FileAttribute 0)))]
    (sh! dir "git" "init" "-q" "-b" "main")
    (sh! dir "git" "config" "user.email" "t@example.com")
    (sh! dir "git" "config" "user.name" "t")
    (sh! dir "git" "config" "commit.gpgsign" "false")
    (sh! dir "git" "config" "tag.gpgsign" "false")
    (put! dir "src/a.clj" "a1")
    (put! dir "test/t.clj" "t1")
    (put! dir ".gitignore" ".switchyard/\n")
    (put! dir ".switchyard/main/lock" "pid")
    (sh! dir "git" "add" ".")
    (sh! dir "git" "commit" "-qm" "init")
    dir))

(deftest branch-and-toplevel
  (let [dir (make-repo)]
    (is (= "main" (sut/branch dir)))
    (is (= (.getCanonicalPath (io/file dir)) (.getCanonicalPath (io/file (sut/toplevel dir)))))
    (sh! dir "git" "checkout" "-q" "--detach")
    (is (nil? (sut/branch dir)) "detached HEAD has no branch")))

(deftest snapshot-is-by-content
  (let [dir (make-repo)
        s0 (sut/snapshot dir)]
    (testing "an unchanged tree snapshots identically"
      (is (= s0 (sut/snapshot dir))))
    (testing "re-editing an already-dirty file changes the snapshot"
      (put! dir "test/t.clj" "dirty-1")
      (let [dirty (sut/snapshot dir)]
        (put! dir "test/t.clj" "dirty-2")
        (is (not= (:tree dirty) (:tree (sut/snapshot dir))))
        (is (= ["test/t.clj"] (sut/changed-paths dir dirty (sut/snapshot dir))))))
    (testing "ignored .switchyard/ is part of the snapshot"
      (let [before (sut/snapshot dir)]
        (put! dir ".switchyard/main/lock" "tampered")
        (is (= [".switchyard/main/lock"]
               (sut/changed-paths dir before (sut/snapshot dir))))))
    (testing "a stat-only index refresh is not a change"
      (let [before (sut/snapshot dir)]
        (sh! dir "touch" "src/a.clj")
        (sh! dir "git" "status")
        (is (empty? (sut/tamper-violations before (sut/snapshot dir))))))))

(deftest tamper-is-detected
  (doseq [[label action what]
          [["commit"  #(do (put! % "src/a.clj" "a2")
                           (sh! % "git" "commit" "-qam" "x")) :head]
           ["stage"   #(do (put! % "src/new.clj" "n")
                           (sh! % "git" "add" "src/new.clj")) :index]
           ["branch switch" #(sh! % "git" "checkout" "-q" "-b" "other") :head-ref]
           ["new ref" #(sh! % "git" "tag" "t") :refs]]]
    (testing label
      (let [dir (make-repo)
            before (sut/snapshot dir)]
        (action dir)
        (is (some #{what} (map :what (sut/tamper-violations before (sut/snapshot dir)))))))))

(deftest restore-reproduces-the-snapshot
  (let [dir (make-repo)
        before (sut/snapshot dir)]
    ;; a mix: in-jurisdiction edit, out-of-jurisdiction edit, new file, deletion,
    ;; file<->dir swap, run-record tamper, plus commit/stage/branch tampering.
    (put! dir "src/a.clj" "edited")
    (put! dir "deps.edn" "new")
    (put! dir "src/deep/new.clj" "n")
    (.delete (io/file dir "test/t.clj"))
    (put! dir ".switchyard/main/lock" "tampered")
    (sh! dir "git" "add" "src/a.clj")
    (sh! dir "git" "commit" "-qm" "sneaky")
    (sh! dir "git" "checkout" "-q" "-b" "other")
    (let [after (sut/snapshot dir)
          now (sut/restore! dir before after)]
      (is (= (:tree before) (:tree now)))
      (is (= (dissoc before :tree) (dissoc now :tree)))
      (is (= "main" (sut/branch dir)))
      (is (= "a1" (slurp (io/file dir "src/a.clj"))))
      (is (= "t1" (slurp (io/file dir "test/t.clj"))))
      (is (not (.exists (io/file dir "deps.edn"))))
      (is (not (.exists (io/file dir "src/deep"))) "empty parents are pruned")
      (is (= "pid" (slurp (io/file dir ".switchyard/main/lock")))))))

(deftest restore-handles-file-dir-swaps
  (let [dir (make-repo)
        before (sut/snapshot dir)]
    (.delete (io/file dir "src/a.clj"))
    (put! dir "src/a.clj/inner.clj" "x")
    (let [after (sut/snapshot dir)]
      (is (= (:tree before) (:tree (sut/restore! dir before after))))
      (is (.isFile (io/file dir "src/a.clj"))))))
