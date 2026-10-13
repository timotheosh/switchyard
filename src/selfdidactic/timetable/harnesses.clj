(ns selfdidactic.timetable.harnesses
  (:require [clojure.spec.alpha :as s]))

(s/def ::command string?)

(s/def ::extra-params (s/coll-of string? :kind vector?))

;; :exit-codes names this harness's exit codes ({137 :oom-killed}); it extends
;; and overrides the plugin's defaults.
;; s/keys is open: harness/plugin-specific keys pass through.
(s/def ::exit-codes (s/map-of nat-int? keyword?))

(s/def ::harness (s/keys :req-un [::command] :opt-un [::extra-params ::exit-codes]))

(s/def ::harnesses (s/map-of keyword? ::harness :min-count 1))
