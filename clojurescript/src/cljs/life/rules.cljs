(ns life.rules
  (:require [life.config :as config]))

(defn p2->1 [[x y]]
  (+ (mod x config/world-size)
     (* config/world-size (mod y config/world-size))))

(defn p1->2 [n]
  [(mod (mod n config/world-size) config/world-size)
   (mod (quot n config/world-size) config/world-size)])

(defn get-p [m p]
  (nth m (p2->1 p)))

(defn set-p [m p v]
  (assoc m (p2->1 p) v))

(defn alive-neighbours [cells [x y]]
  (let [offsets [[-1 -1] [0 -1] [1 -1]
                 [-1 0]         [1 0]
                 [-1 1]  [0 1]  [1 1]]]
    (reduce
     (fn [alive [ox oy]]
       (if (get-p cells [(+ x ox) (+ y oy)]) (inc alive) alive))
     0
     offsets)))

(defn step [cells]
  (map-indexed
   (fn [n alive?]
     (let [pos (p1->2 n)
           neighbours (alive-neighbours cells pos)]
       (cond
         (and alive? (< neighbours 2)) false
         (and alive? (> neighbours 3)) false
         (and alive? (or (= 2 neighbours) (= 3 neighbours))) true
         (and (not alive?) (= 3 neighbours)) true
         :else alive?)))
   cells))
