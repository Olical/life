(ns life.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clojure.string :as s]))

(def cell-size 10)
(def board-size 100)

(def state->color {0 [255 255 255]
                   1 [0 0 0]})

(def initial-board (into [] (repeat (* board-size board-size) 0)))

(defn random-board []
  (into
   []
   (take (* board-size board-size) (repeatedly #(Math/round (rand))))))

(defn print-board [board]
  (let [lines (partition board-size board)
        flat (s/join "\n" (map #(s/join " " %) lines))]
    (println flat)))

(defn p2->1 [[x y]]
  (+ (mod x board-size)
     (* board-size (mod y board-size))))

(defn p1->2 [n]
  [(mod (mod n board-size) board-size)
   (mod (quot n board-size) board-size)])

(defn get-p [m p]
  (get m (p2->1 p)))

(defn set-p [m p v]
  (assoc m (p2->1 p) v))

(defn add-glider [board]
  (-> board
      (set-p [1 0] 1)
      (set-p [2 1] 1)
      (set-p [2 2] 1)
      (set-p [0 2] 1)
      (set-p [1 2] 1)))

(defn alive-neighbours [board [x y]]
  (let [offsets [[-1 -1] [0 -1] [1 -1]
                 [-1 0]         [1 0]
                 [-1 1]  [0 1]  [1 1]]]
    (reduce
     (fn [alive [ox oy]]
       (if (= 1 (get-p board [(+ x ox) (+ y oy)])) (inc alive) alive))
     0
     offsets)))

(defn key-pressed [state event]
  (case (event :key-code)
    32 (update state :paused not) ;; space
    82 (assoc state :board (random-board)) ;; r
    state))

(def flip-life {0 1 1 0})

(defn mouse-clicked [state event]
  (if (= :left (:button event))
    (let [x (quot (:x event) cell-size)
          y (quot (:y event) cell-size)]
      (update-in state [:board (p2->1 [x y])] flip-life))
    state))

(defn setup []
  (q/frame-rate 10)
  (q/text-font (q/create-font "Sans Serif" 28 true))
  {:board (add-glider initial-board)
   :paused false})

;; Any live cell with fewer than two live neighbours dies, as if caused by under-population.
;; Any live cell with two or three live neighbours lives on to the next generation.
;; Any live cell with more than three live neighbours dies, as if by over-population.
;; Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.

(defn step [board]
  (into
   []
   (map-indexed
    (fn [n state]
      (let [pos (p1->2 n)
            neighbours (alive-neighbours board pos)
            alive? (= 1 state)]
        (cond
          (and alive? (< neighbours 2)) 0
          (and alive? (> neighbours 3)) 0
          (and alive? (or (= 2 neighbours) (= 3 neighbours))) 1
          (and (not alive?) (= 3 neighbours)) 1
          :else state)))
    board)))

(defn update-state [state]
  (if (:paused state)
    state
    (update state :board step)))

(defn draw-state [state]
  (q/stroke 200 200 200)
  (doall
   (for [x (range board-size)
         y (range board-size)]
     (let [cell (get-p (:board state) [x y])
           color (state->color cell)]
       (apply q/fill color)
       (q/rect (* x cell-size)
               (* y cell-size)
               cell-size
               cell-size))))
  (when (:paused state)
   (q/no-stroke)
   (q/fill 0 0 0 150)
   (q/rect 0 0 130 40)
   (q/fill 255 255 255)
   (q/text "PAUSED" 8 29)))

(defn start []
  (q/defsketch life
    :title "Game of life"
    :size (let [size (* board-size cell-size)] [size size])
    :setup setup
    :update update-state
    :draw draw-state
    :key-pressed key-pressed
    :mouse-clicked mouse-clicked
    :features [:keep-on-top]
    :middleware [m/fun-mode]))

(defn -main []
  (start))
