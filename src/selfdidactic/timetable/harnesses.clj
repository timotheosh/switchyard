(ns selfdidactic.timetable.harnesses
  (:require [clojure.spec.alpha :as s]))

(s/def ::command string?)

;; s/keys is open: harness/plugin-specific keys pass through.
(s/def ::harness (s/keys :req-un [::command]))

(s/def ::harnesses (s/map-of keyword? ::harness :min-count 1))
