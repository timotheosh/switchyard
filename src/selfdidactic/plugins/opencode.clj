(ns selfdidactic.plugins.opencode
  "Turn an opencode harness/agent/prompt into the opencode command line."
  (:require [selfdidactic.plugins.flags :as flags]))

(defn build-argv
  "Calculate the opencode command line for running one agent against a prompt.

  Always runs via `opencode run`, which is non-interactive. opencode has no
  skills flag, so a harness without a :skill mapping rejects an agent that
  supplies skills. The prompt follows -- because opencode would otherwise
  parse a prompt starting with - as a flag."
  [harness agent prompt]
  (when-not (and (string? prompt) (seq prompt))
    (throw (ex-info "Run requires a non-empty prompt" {:prompt prompt})))
  (vec (concat [(:command harness) "run"]
               (flags/flag-args (:model harness) (:model agent))
               (flags/flag-args (:skill harness) (:skills agent))
               ["--" prompt])))
