(ns selfdidactic.plugins.fake
  "Test plugin: the agent's :model is the shell script to run, and the prompt
  arrives on stdin.")

(defn build-command [_harness agent prompt]
  {:argv ["sh" "-c" (:model agent)] :stdin prompt})

(def exit-reasons {3 :boom})
