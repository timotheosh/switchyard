(ns selfdidactic.switchyard
  (:gen-class)
  (:require [selfdidactic.cli :as cli]))

(defn -main
  "Run the non-interactive command-line interface."
  [& args]
  (let [exit-code (cli/run-cli! args)]
    (when-not (zero? exit-code)
      (System/exit exit-code))))
