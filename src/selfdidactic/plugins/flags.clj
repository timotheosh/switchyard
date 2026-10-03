(ns selfdidactic.plugins.flags
  "Turn a harness's CLI-parameter mapping and a value into argv elements.")

(defn- arg-str
  "Calculate the CLI string for a value, keeping any keyword namespace."
  [v]
  (if (keyword? v) (subs (str v) 1) (str v)))

(defn flag-args
  "Calculate the CLI args for one parameter. A nil or empty-collection value
  supplies nothing; a supplied value with no mapping is a configuration bug."
  [{:keys [parameter plural]} value]
  (cond
    (or (nil? value) (and (coll? value) (empty? value))) []
    (nil? parameter) (throw (ex-info "Harness has no mapping for a supplied value"
                                      {:value value}))
    plural           (mapcat (fn [v] [parameter (arg-str v)]) value)
    :else            [parameter (arg-str value)]))
