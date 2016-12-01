(ns life.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clojure.string :as s]))

(def cell-size 10)
(def board-size 30)

(def state->color {0 [255 255 255]
                   1 [0 0 0]})

(def initial-board (into [] (repeat (* board-size board-size) 0)))

(defn print-board [board]
  (let [lines (partition board-size board)
        flat (s/join "\n" (map #(s/join " " %) lines))]
    (println flat)))

(defn add-glider [board]
  (-> board
      (assoc (pos 1 0) 1)
      (assoc (pos 2 1) 1)
      (assoc (pos 2 2) 1)
      (assoc (pos 0 2) 1)
      (assoc (pos 1 2) 1)))

(defn setup []
  (q/frame-rate 1)
  {:board (add-glider initial-board)})

(defn update-state [state]
  state)

(defn pos [x y]
  (+ x (* board-size y)))

(defn draw-state [state]
  (q/no-stroke)
  (doall
   (for [x (range board-size)
         y (range board-size)]
     (let [cell (get (:board state) (pos x y))
           color (state->color cell)]
       (apply q/fill color)
       (q/rect (* x cell-size)
               (* y cell-size)
               cell-size
               cell-size)))))

(q/defsketch life
  :title "Game of life"
  :size (let [size (* board-size cell-size)] [size size])
  :setup setup
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  :middleware [m/fun-mode])
