(ns selfdidactic.timetable
  "A set of timetable EDN files is one logical timetable. File names and
  boundaries carry no meaning: fragments are deep-merged in sorted-name
  order and only the merged result is validated."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [selfdidactic.timetable.agents :as agents]
            [selfdidactic.timetable.harnesses :as harnesses]
            [selfdidactic.timetable.pipelines :as pipelines]
            [selfdidactic.timetable.prompts :as prompts]
            [selfdidactic.timetable.skills :as skills]))

(def root-specs
  "The closed vocabulary of root keys, and the spec for each."
  {:agents    ::agents/agents
   :harnesses ::harnesses/harnesses
   :pipelines ::pipelines/pipelines
   :prompts   ::prompts/prompts
   :skills    ::skills/skills})

(def required-roots #{:agents :harnesses :pipelines :prompts})

(defn deep-merge
  "Calculate the merge of two values. Maps merge recursively; otherwise the
  later value wins (vectors are replaced, never concatenated; nil replaces)."
  [a b]
  (if (and (map? a) (map? b))
    (merge-with deep-merge a b)
    b))

(defn merge-timetables
  "Calculate one timetable from fragments; later fragments take precedence."
  [& fragments]
  (reduce deep-merge {} fragments))

(defn- spec-problems [root value]
  (for [p (::s/problems (s/explain-data (root-specs root) value))]
    {:problem :invalid
     :root    root
     :path    (:path p)
     :pred    (:pred p)
     :val     (:val p)}))

(defn problems
  "Calculate the validation problems of a merged timetable; empty when valid."
  [timetable]
  (concat
   (for [k (keys timetable) :when (not (contains? root-specs k))]
     {:problem :unknown-root-key :root k})
   (for [k (sort required-roots) :when (not (contains? timetable k))]
     {:problem :missing-root :root k})
   (for [[k v] timetable :when (contains? root-specs k)
         p (spec-problems k v)]
     p)))

(defn valid? [timetable]
  (empty? (problems timetable)))

(defn validate
  "Return the timetable, or throw ex-info whose data lists the :problems."
  [timetable]
  (let [ps (vec (problems timetable))]
    (if (empty? ps)
      timetable
      (throw (ex-info (str "Invalid timetable: " (count ps) " problem(s)")
                      {:problems ps})))))

(defn load-fragment
  "Read one EDN file: any subset of a timetable, not validated on its own."
  [path]
  (edn/read-string (slurp path)))

(defn- edn-files [dir]
  (->> (.listFiles (io/file dir))
       (filter #(and (.isFile %) (.endsWith (.getName %) ".edn")))
       (sort-by #(.getName %))))

(defn load-dir
  "Load every *.edn in dir (sorted by name), merge, and validate."
  [dir]
  (->> (edn-files dir)
       (map load-fragment)
       (apply merge-timetables)
       validate))
