(ns life.db
  (:require [life.config :as config]))

(def default-db
  (let [world-size config/world-size
        cells (repeatedly (* world-size world-size) #(rand-nth [true false]))]
    {:cells cells}))
