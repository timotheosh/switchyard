(ns selfdidactic.config
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [config.core :as yogthos]
            [toml-clj.core :as toml])
  (:import [java.io File]))

(def default-resource "switchyard.toml")

(def required-keys
  "Each required config key, and how it can be supplied."
  {:spec {:flag "--spec" :toml "spec"}})

(defn toml->edn
  "Calculate an EDN map, with keyword keys, from TOML text."
  [toml-str]
  (or (toml/read-string toml-str {:key-fn keyword}) {}))

(defn show-str
  "Calculate the EDN text of a config."
  [config]
  (pr-str config))

(defn merge-overrides
  "Calculate a config where CLI overrides win over the file config."
  [file-config overrides]
  (merge file-config overrides))

(defn missing-keys
  "Calculate the required keys absent from a config."
  [required config]
  (remove #(contains? config %) (keys required)))

(defn describe-missing
  "Calculate a message naming each missing key and how it can be supplied."
  [required missing]
  (str "Missing required configuration:\n"
       (str/join "\n"
                 (for [k missing
                       :let [{:keys [flag toml]} (get required k)]]
                   (str "  " (name k) ": " flag " or `" toml
                        "` in the config file")))))

(defn source-label
  "Calculate a display name for where the file config comes from."
  [path resource-name]
  (or path (str "resource " resource-name)))

(defn key-sources
  "Calculate, for each key in the effective config, where it came from."
  [overrides file-config label]
  (into {}
        (concat (for [k (keys file-config)] [k label])
                (for [k (keys overrides)] [k (str "--" (name k))]))))

(defn resolve-source
  "Find the file config location: an explicit file is required; the default
  resource may be absent (nil location)."
  [path resource-name]
  (if path
    {:location (io/file path) :required? true}
    {:location (io/resource resource-name) :required? false}))

(defn- exists? [location]
  (if (instance? File location) (.isFile ^File location) (some? location)))

(defn- read-via-yogthos!
  "Load an EDN map through yogthos/config. This is the only use of config.core."
  [config]
  (let [file (File/createTempFile "switchyard-config-" ".edn")]
    (try
      (spit file (show-str config))
      (yogthos/read-config-file (str file))
      (finally (.delete file)))))

(defn load-config!
  "Read the TOML config file (or the default resource when path is nil) as a
  plain map of the file's own keys."
  ([path] (load-config! path default-resource))
  ([path resource-name]
   (let [{:keys [location required?]} (resolve-source path resource-name)]
     (cond
       (exists? location)
       (let [parsed (try (toml->edn (slurp location))
                         (catch Exception e
                           (throw (ex-info (str "invalid TOML: " (.getMessage e))
                                           {:kind :invalid-config :path path}
                                           e))))]
         (or (read-via-yogthos! parsed) {}))

       required?
       (throw (ex-info "config file not found or not a regular file"
                       {:kind :missing-config :path path}))

       :else {}))))
