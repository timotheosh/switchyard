(ns selfdidactic.plugins
  "Find the pieces of the configured plugin: the config's `plugin` key names a
  namespace under selfdidactic.plugins that defines `build-command`, returning
  {:argv [...] :stdin text-or-nil}, and optionally `exit-reasons`, a map from
  the harness's exit codes to reason keywords.")

(defn- plugin-ns
  "Calculate the plugin namespace name for the config."
  [config]
  (if-let [plugin (:plugin config)]
    (str "selfdidactic.plugins." plugin)
    (throw (ex-info "No plugin configured; set `plugin` in the config file"
                    {:kind :missing-plugin}))))

(defn builder-symbol
  "Calculate the qualified `build-command` symbol for the config's plugin."
  [config]
  (symbol (plugin-ns config) "build-command"))

(defn build-command!
  "Load the configured plugin and return its command for agent and prompt."
  [config harness agent prompt]
  (let [sym (builder-symbol config)
        f   (try (requiring-resolve sym)
                 (catch java.io.FileNotFoundException e
                   (throw (ex-info (str "Unknown plugin: " (:plugin config))
                                   {:kind :unknown-plugin :symbol sym} e))))]
    (f harness agent prompt)))

(defn exit-reasons
  "Calculate {exit-code reason} for a harness: the plugin's defaults, extended
  and overridden by the harness's own :exit-codes."
  [config harness]
  (merge {}
         (some-> (symbol (plugin-ns config) "exit-reasons") requiring-resolve deref)
         (:exit-codes harness)))
