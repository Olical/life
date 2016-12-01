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

(defn pos [[x y]]
  (+ x (* board-size y)))

(defn get-p [m p]
  (get m (pos p)))

(defn set-p [m p v]
  (assoc m (pos p) v))

(defn add-glider [board]
  (-> board
      (set-p [1 0] 1)
      (set-p [2 1] 1)
      (set-p [2 2] 1)
      (set-p [0 2] 1)
      (set-p [1 2] 1)))

(defn setup []
  (q/frame-rate 1)
  {:board (add-glider initial-board)})

(defn step [board]
  board)
  ;; (loop [next initial-board
  ;;        x 0
  ;;        y 0]
  ;;   (if (= x y board-size)
  ;;     next
  ;;     (recur))))

(defn update-state [state]
  (update state :board step))

(defn draw-state [state]
  (q/no-stroke)
  (doall
   (for [x (range board-size)
         y (range board-size)]
     (let [cell (get-p (:board state) [x y])
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
