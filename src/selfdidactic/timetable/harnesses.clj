(ns selfdidactic.timetable.harnesses
  (:require [clojure.spec.alpha :as s]))

(s/def ::command string?)

(s/def ::extra-params (s/coll-of string? :kind vector?))

;; s/keys is open: harness/plugin-specific keys pass through.
(s/def ::harness (s/keys :req-un [::command] :opt-un [::extra-params]))

(s/def ::harnesses (s/map-of keyword? ::harness :min-count 1))
