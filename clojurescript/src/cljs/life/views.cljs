(ns life.views
    (:require [re-frame.core :as re-frame]))

(def cell-state->class
  {true "cell-alive"
   false "cell-dead"})

(defn cell [i c]
  [:div.cell {:class (cell-state->class c) :key (str "cell-" i)}])

(defn main-panel []
  (let [cells (re-frame/subscribe [:cells])]
    (fn [] [:div.cells (map-indexed cell @cells)])))
