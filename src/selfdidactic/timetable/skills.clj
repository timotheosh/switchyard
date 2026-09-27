(ns selfdidactic.timetable.skills
  (:require [clojure.spec.alpha :as s]))

(s/def ::path string?)

(s/def ::skill (s/keys :req-un [::path]))

(s/def ::skills (s/map-of keyword? ::skill))
