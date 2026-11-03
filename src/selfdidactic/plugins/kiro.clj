(ns selfdidactic.plugins.kiro
  "Turn a kiro-cli harness/agent/prompt into the kiro-cli command."
  (:require [selfdidactic.plugins.flags :as flags]))

(def exit-reasons
  "What kiro-cli's exit codes mean. 1, 3 and 4 are from Kiro's exit-codes
  reference (kiro.dev/docs/reference/exit-codes); 1 was also probed (unknown
  model). 143 is SIGTERM as plain signal death (probed). A tool call that
  isn't pre-approved is denied and the run continues with exit 0."
  {1   :error
   3   :mcp-startup-failed
   4   :agent-not-found
   143 :interrupted})

(defn build-command
  "Calculate the kiro-cli command for running one agent against a prompt:
  {:argv [...] :stdin prompt}.

  Always runs `kiro-cli chat --no-interactive`. The prompt goes on stdin, which
  avoids argv length limits and prompts that look like flags. kiro-cli has no
  skills flag, so a harness without a :skill mapping rejects an agent that
  supplies skills. The harness's :extra-params (tool trust, --agent) are passed
  through verbatim."
  [harness agent prompt]
  (when-not (and (string? prompt) (seq prompt))
    (throw (ex-info "Run requires a non-empty prompt" {:prompt prompt})))
  {:argv (vec (concat [(:command harness) "chat" "--no-interactive"]
                      (flags/flag-args (:model harness) (:model agent))
                      (flags/flag-args (:skill harness) (:skills agent))
                      (:extra-params harness)))
   :stdin prompt})
