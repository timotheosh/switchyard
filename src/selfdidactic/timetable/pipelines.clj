(ns selfdidactic.timetable.pipelines
  (:require [clojure.spec.alpha :as s]))

(s/def ::agent keyword?)

;; Open: the step DSL (:prompt, :callback, :thread, ...) is not validated here.
(s/def ::step (s/keys :req-un [::agent]))

(s/def ::steps (s/coll-of ::step :kind vector? :min-count 1))

(s/def ::pipeline (s/keys :req-un [::steps]))

(s/def ::pipelines (s/map-of keyword? ::pipeline :min-count 1))
