(ns selfdidactic.timetable.prompts
  (:require [clojure.spec.alpha :as s]))

(s/def ::path string?)

(s/def ::prompt (s/keys :req-un [::path]))

(s/def ::prompts (s/map-of keyword? ::prompt :min-count 1))
