(ns selfdidactic.progress
  "The live progress line on the terminal: a calculation that renders one stage
  event as a bar, and the action that draws it in place on a stream."
  (:require [clojure.string :as str])
  (:import [java.io PrintStream]))

(def ^:private bar-width 20)
(def ^:private clear-line "\r\u001b[2K")

(defn render
  "Calculate the progress line for a stage event {:index :total :agent :phase},
  :phase being :running or :done. The bar fills as stages complete."
  [{:keys [index total agent phase]}]
  (let [completed (if (= :done phase) (inc index) index)
        filled    (quot (* bar-width completed) total)]
    (str "[" (apply str (repeat filled "="))
         (apply str (repeat (- bar-width filled) " "))
         "] stage " (inc index) "/" total " " (name agent) " " (name phase))))

(def silent
  "A display that draws nothing, for headless runs."
  {:clear! (constantly nil) :draw! (constantly nil)})

(defn display
  "A display that redraws one line on `out`. Call :clear! before anything else
  writes to the stream so the line never interleaves with log output."
  [^PrintStream out]
  {:clear! (fn [] (.print out clear-line) (.flush out))
   :draw!  (fn [event] (.print out (render event)) (.flush out))})
