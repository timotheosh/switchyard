(ns selfdidactic.timetable.agents
  (:require [clojure.spec.alpha :as s]))

(s/def ::harness keyword?)
(s/def ::model string?)
(s/def ::skills (s/coll-of keyword? :kind vector?))

(s/def ::agent (s/keys :req-un [::harness ::model] :opt-un [::skills]))

(s/def ::agents (s/map-of keyword? ::agent :min-count 1))
