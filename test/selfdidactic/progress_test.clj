(ns selfdidactic.progress-test
  (:require [clojure.test :refer [deftest is]]
            [selfdidactic.pipeline :as pipeline]
            [selfdidactic.progress :as sut]))

(deftest render-fills-as-stages-complete
  (is (= "[=====               ] stage 2/4 writer running"
         (sut/render {:index 1 :total 4 :agent :writer :phase :running})))
  (is (= "[==========          ] stage 2/4 writer done"
         (sut/render {:index 1 :total 4 :agent :writer :phase :done}))))

(deftest stage-messages-are-distinct-start-and-end
  (is (= "stage 1/2 writer started" (pipeline/stage-started-message 0 2 :writer)))
  (is (= "stage 1/2 writer finished: ok" (pipeline/stage-finished-message 0 2 :writer :ok))))
