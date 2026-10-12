(ns selfdidactic.git
  "The orchestrator's git actions. Switchyard runs git only around agent
  handoffs: it snapshots the working tree by content before and after a step,
  and can restore the whole tree, refs, HEAD and index to a snapshot."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [selfdidactic.harness :as harness])
  (:import [java.io File]))

(defn split-nul
  "Calculate the strings of NUL-separated git output."
  [s]
  (remove str/blank? (str/split s #"\u0000")))

(defn parse-refs
  "Calculate a {refname sha} map from `for-each-ref` lines."
  [out]
  (into {} (for [line (str/split-lines out)
                 :let [[ref sha] (str/split line #" ")]
                 :when sha]
             [ref sha])))

(defn tamper-violations
  "Calculate the :git-tamper violations between two snapshots: an agent may
  not commit, reset, switch branches or stage."
  [before after]
  (vec (for [k [:head :head-ref :refs :index]
             :when (not= (k before) (k after))]
         {:violation :git-tamper :what k})))

(defn- git!
  "Run git in dir with optional env and stdin; return stdout or throw."
  [dir {:keys [env stdin]} & args]
  (let [{:keys [process out err]} (harness/start (into ["git"] args) dir
                                                  {:env env :stdin stdin})]
    (if (= :ok process)
      out
      (throw (ex-info (str "git " (str/join " " (take 2 args)) " failed: "
                           (str/trim err))
                      {:kind :git-failed :args args})))))

(defn- git-line [dir & args]
  (str/trim (apply git! dir {} args)))

(defn- git-line-or-nil
  "Like git-line, but nil when git exits non-zero (e.g. no commits, detached)."
  [dir & args]
  (let [{:keys [process out]} (harness/start (into ["git"] args) dir)]
    (when (= :ok process) (str/trim out))))

(defn toplevel
  "The repository's top-level directory for dir."
  [dir]
  (git-line dir "rev-parse" "--show-toplevel"))

(defn branch
  "The current branch name, or nil on a detached HEAD."
  [dir]
  (git-line-or-nil dir "symbolic-ref" "--short" "-q" "HEAD"))

(defn snapshot
  "Take a content snapshot of repo root dir: the tree of every file in the
  working tree (tracked, untracked and dirty, plus .switchyard/ even when
  ignored), built in a temporary index so the real index is untouched. Also
  records HEAD, its branch ref, all refs, and the real index by content."
  [dir]
  (let [index (File/createTempFile "switchyard-index-" ".tmp")]
    (.delete index)
    (try
      (let [opts {:env {"GIT_INDEX_FILE" (str index)}}]
        (git! dir opts "add" "-A")
        (when (.exists (File. ^String dir ".switchyard"))
          (git! dir opts "add" "-f" "--" ".switchyard"))
        {:tree     (str/trim (git! dir opts "write-tree"))
         :head     (git-line-or-nil dir "rev-parse" "--verify" "-q" "HEAD")
         :head-ref (git-line-or-nil dir "symbolic-ref" "-q" "HEAD")
         :refs     (parse-refs (git! dir {} "for-each-ref"
                                     "--format=%(refname) %(objectname)"))
         :index    (git-line dir "write-tree")})
      (finally (.delete index)))))

(defn changed-paths
  "The paths whose content differs between two snapshots' trees."
  [dir before after]
  (if (= (:tree before) (:tree after))
    []
    (vec (split-nul (git! dir {} "diff" "--name-only" "-z" "--no-renames"
                          (:tree before) (:tree after))))))

(defn- restore-refs! [dir before after]
  (doseq [ref (set/difference (set (keys (:refs after))) (set (keys (:refs before))))]
    (git! dir {} "update-ref" "-d" ref))
  (doseq [[ref sha] (:refs before) :when (not= sha (get (:refs after) ref))]
    (git! dir {} "update-ref" ref sha))
  (when (and (:head-ref before) (not= (:head-ref before) (:head-ref after)))
    (git! dir {} "symbolic-ref" "HEAD" (:head-ref before))))

(defn- delete-with-empty-parents! [dir path]
  (let [root (File. ^String dir)]
    (loop [f (File. root ^String path)]
      (when (and (not= f root) (or (.isFile f) (and (.isDirectory f) (empty? (.list f)))))
        (.delete f)
        (recur (.getParentFile f))))))

(defn- restore-worktree! [dir {:keys [tree]} paths]
  (let [in-tree (set (split-nul (git! dir {} "ls-tree" "-r" "--name-only" "-z" tree)))
        {kept true gone false} (group-by (comp boolean in-tree) paths)]
    ;; Deletions first: a path may have changed between file and directory.
    (doseq [p gone] (delete-with-empty-parents! dir p))
    (when (seq kept)
      (git! dir {:env {"GIT_LITERAL_PATHSPECS" "1"}
                 :stdin (str/join "\u0000" kept)}
            "restore" (str "--source=" tree) "--worktree"
            "--pathspec-from-file=-" "--pathspec-file-nul"))))

(defn restore!
  "Restore the whole step to the `before` snapshot: refs, HEAD and index when
  tampered, and every path the step changed (not only the violating ones, so
  no half-applied state remains). Throws if the tree is not identical after."
  [dir before after]
  (restore-refs! dir before after)
  (when (not= (:index before) (:index after))
    (git! dir {} "read-tree" (:index before)))
  (restore-worktree! dir before (changed-paths dir before after))
  (let [now (snapshot dir)]
    (when (not= (:tree before) (:tree now))
      (throw (ex-info "Restore did not reproduce the snapshot"
                      {:kind :restore-failed :expected (:tree before) :actual (:tree now)})))
    now))
