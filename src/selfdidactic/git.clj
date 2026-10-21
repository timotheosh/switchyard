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

(def ^:private volatile-paths
  "Pathspecs a snapshot never includes: run worktrees, and the reserved
  orchestrator state directory (`db/`), which must never be rolled back under
  an open memory-mapped database. `add -f` ignores .gitignore, so they are
  removed from the temporary index instead."
  [".switchyard/worktrees" ":(glob).switchyard/*/db/**"])

(defn- tree-of
  "The tree of the files selected by `adds` (a seq of `git add` argument
  vectors) less the `removes` pathspecs, built in a temporary index in dir so
  the real index is untouched."
  [dir adds removes]
  (let [index (File/createTempFile "switchyard-index-" ".tmp")]
    (.delete index)
    (try
      (let [opts {:env {"GIT_INDEX_FILE" (str index)}}]
        (doseq [args adds] (apply git! dir opts "add" args))
        (when (seq adds)
          (apply git! dir opts "rm" "-r" "-f" "--cached" "-q" "--ignore-unmatch" "--" removes))
        (str/trim (git! dir opts "write-tree")))
      (finally (.delete index)))))

(defn snapshot
  "Take a content snapshot of repo root dir: the tree of every file in the
  working tree (tracked, untracked and dirty, plus .switchyard/ even when
  ignored, minus worktrees and `db/`), built in a temporary index. Also
  records HEAD, its branch ref, all refs, and the real index by content."
  [dir]
  {:tree     (tree-of dir
                      (cond-> [["-A" "--" "."]]
                        (.exists (File. ^String dir ".switchyard"))
                        (conj ["-f" "--" ".switchyard"]))
                      volatile-paths)
   :head     (git-line-or-nil dir "rev-parse" "--verify" "-q" "HEAD")
   :head-ref (git-line-or-nil dir "symbolic-ref" "-q" "HEAD")
   :refs     (parse-refs (git! dir {} "for-each-ref"
                               "--format=%(refname) %(objectname)"))
   :index    (git-line dir "write-tree")})

(defn watch-snapshot
  "Take the snapshots of what an agent running in a worktree must not touch in
  the main checkout `root`, for branch directory `eb`: :state (the runner's
  own `.switchyard/<eb>`, minus `db/`) and :main (everything else in root
  except worktrees and `db/`)."
  [root eb]
  (let [state (str ".switchyard/" eb)]
    {:state (tree-of root
                     (when (.exists (File. ^String root ^String state))
                       [["-f" "--" state]])
                     [(str state "/db")])
     :main  (tree-of root [["-A" "--" "."]]
                     (conj volatile-paths state))}))

(defn dir-trees
  "The tree hash of each directory prefix in a snapshot, {prefix hash}. An
  absent or empty directory is nil: git has no empty trees, and `rev-parse`
  exits non-zero for a path the tree lacks."
  [dir snapshot prefixes]
  (into (sorted-map)
        (for [p prefixes]
          [p (git-line-or-nil dir "rev-parse" "--verify" "-q"
                              (str (:tree snapshot) ":" (str/replace p #"/+$" "")))])))

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

(defn restore-tree!
  "Restore the files that differ between two watcher trees of dir to `before`."
  [dir before-tree after-tree]
  (restore-worktree! dir {:tree before-tree}
                     (changed-paths dir {:tree before-tree} {:tree after-tree})))

(defn- sha256-file [^File f]
  (apply str (map #(format "%02x" %)
                  (.digest (java.security.MessageDigest/getInstance "SHA-256")
                           (java.nio.file.Files/readAllBytes (.toPath f))))))

(defn common-dir
  "The git directory shared by every worktree of dir's repository."
  [dir]
  (let [f (File. (git-line dir "rev-parse" "--git-common-dir"))]
    (if (.isAbsolute f) f (File. ^String dir (.getPath f)))))

(defn git-dir-hashes
  "Hash the files an agent could plant a delayed payload in: the shared git
  directory's config, hooks/ and info/. {relative-path sha256}."
  [dir]
  (let [gd (common-dir dir)
        files (concat [(File. gd "config")]
                      (for [sub ["hooks" "info"], f (file-seq (File. gd ^String sub))] f))]
    (into (sorted-map)
          (for [^File f files :when (.isFile f)]
            [(str (.relativize (.toPath gd) (.toPath f))) (sha256-file f)]))))

(defn dirty-paths
  "The tracked paths of dir with uncommitted changes (untracked files and
  .switchyard/ are not considered)."
  [dir]
  (mapv #(subs % 3)
        (split-nul (git! dir {} "status" "--porcelain" "-z" "--no-renames"
                         "--untracked-files=no" "--" "." ":(exclude).switchyard"))))

(defn branch-exists?
  "Whether a local branch of that name exists in dir's repository."
  [dir branch]
  (some? (git-line-or-nil dir "show-ref" "--verify" "-q" (str "refs/heads/" branch))))

(defn worktree-registered?
  "Whether `path` is a registered worktree of dir's repository."
  [dir path]
  (let [canon #(.getCanonicalPath (File. ^String %))
        target (canon path)]
    (boolean (some #(= target (canon (subs % 9)))
                   (filter #(str/starts-with? % "worktree ")
                           (str/split-lines (git! dir {} "worktree" "list" "--porcelain")))))))

(defn worktree-add!
  "Create a worktree at `path` on a new branch started from HEAD."
  [dir path branch]
  (git! dir {} "worktree" "add" "-q" "-b" branch path "HEAD"))
