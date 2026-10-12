(ns selfdidactic.plugins
  "Find the command builder of the configured plugin: the config's `plugin`
  key names a namespace under selfdidactic.plugins that defines
  `build-command`, returning {:argv [...] :stdin text-or-nil}.")

(defn builder-symbol
  "Calculate the qualified `build-command` symbol for the config's plugin."
  [config]
  (if-let [plugin (:plugin config)]
    (symbol (str "selfdidactic.plugins." plugin) "build-command")
    (throw (ex-info "No plugin configured; set `plugin` in the config file"
                    {:kind :missing-plugin}))))

(defn build-command!
  "Load the configured plugin and return its command for agent and prompt."
  [config harness agent prompt]
  (let [sym (builder-symbol config)
        f   (try (requiring-resolve sym)
                 (catch java.io.FileNotFoundException e
                   (throw (ex-info (str "Unknown plugin: " (:plugin config))
                                   {:kind :unknown-plugin :symbol sym} e))))]
    (f harness agent prompt)))
