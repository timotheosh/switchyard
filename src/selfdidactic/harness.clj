(ns selfdidactic.harness
  "Run a harness command line as a subprocess and classify the outcome."
  (:require [clojure.java.shell :as shell]))

(defn classify
  "Calculate the result message for a completed process run."
  [{:keys [exit out err]}]
  (cond-> {:status (if (zero? exit) :success :error)
           :exit-code exit :out out :err err}
    (not (zero? exit))
    (assoc :reason (case exit 1 :no-output 2 :failed 130 :interrupted :unknown))))

(defn start
  "Run argv as a subprocess in dir and return its classified result."
  [argv dir]
  (when (nil? dir)
    (throw (ex-info "start requires a working directory" {:dir dir})))
  (classify (apply shell/sh (concat argv [:dir dir]))))
