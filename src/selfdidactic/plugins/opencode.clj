(ns selfdidactic.plugins.opencode
  "Turn an opencode harness/agent/prompt into the opencode command."
  (:require [selfdidactic.plugins.flags :as flags]))

(defn build-command
  "Calculate the opencode command for running one agent against a prompt:
  {:argv [...] :stdin prompt}.

  Always runs via `opencode run`, which is non-interactive. The prompt goes on
  stdin: opencode reads its message from there, which avoids argv length
  limits and prompts that look like flags. opencode has no skills flag, so a
  harness without a :skill mapping rejects an agent that supplies skills. The
  harness's :extra-params are passed through verbatim."
  [harness agent prompt]
  (when-not (and (string? prompt) (seq prompt))
    (throw (ex-info "Run requires a non-empty prompt" {:prompt prompt})))
  {:argv (vec (concat [(:command harness) "run"]
                      (flags/flag-args (:model harness) (:model agent))
                      (flags/flag-args (:skill harness) (:skills agent))
                      (:extra-params harness)))
   :stdin prompt})
