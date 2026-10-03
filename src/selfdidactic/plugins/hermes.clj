(ns selfdidactic.plugins.hermes
  "Turn a hermes harness/agent/prompt into the hermes command line."
  (:require [selfdidactic.plugins.flags :as flags]))

(defn build-argv
  "Calculate the hermes command line for running one agent against a prompt.

  Always runs via -z/--oneshot: that's how this harness invokes hermes
  headlessly, not a per-timetable-entry choice, so it's fixed here rather
  than driven by harness data. The joined --oneshot=value form is required
  because a single-word prompt starting with - would otherwise be
  misparsed by argparse as another flag."
  [harness agent prompt]
  (when-not (and (string? prompt) (seq prompt))
    (throw (ex-info "Oneshot run requires a non-empty prompt" {:prompt prompt})))
  (vec (concat [(:command harness)]
               (flags/flag-args (:model harness) (:model agent))
               (flags/flag-args (:skill harness) (:skills agent))
               [(str "--oneshot=" prompt)])))
