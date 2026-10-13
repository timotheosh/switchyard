(ns selfdidactic.plugins.fake
  "Test plugin: the harness's :command is the shell to run, the agent's :model
  is the script, and the prompt arrives on stdin.")

(defn build-command [harness agent prompt]
  {:argv [(:command harness) "-c" (:model agent)] :stdin prompt})

(def exit-reasons {3 :boom})
