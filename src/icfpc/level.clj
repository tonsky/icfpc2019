(ns icfpc.level
  (:require [icfpc.core :refer :all]
            [icfpc.parser :refer :all]))

(defn coord->idx [x y] (+ x (* y width)))

(defn at-coord [level x y]
  (nth level (coord->idx x y)))

(defn mark-level
  ([level x y]
    (if (or (neg? x) (neg? y) (>= x width) (>= y height))
      level
      (assoc level (coord->idx x y) WRAPPED)))
  ([level x y shape]
    (reduce (fn [level [dx dy]] (mark-level level (+ x dx) (+ y dy))) level shape)))


(defn load-level [name]
  {:level/w 8
   :level/h 3
   :level/grid [EMPTY EMPTY EMPTY EMPTY EMPTY OBSTACLE OBSTACLE
                EMPTY EMPTY EMPTY EMPTY EMPTY EMPTY EMPTY
                EMPTY EMPTY EMPTY EMPTY EMPTY OBSTACLE OBSTACLE]
   :bot/x 0
   :bot/y 0
   :bot/layout [[0 0] [1 0] [1 1] [1 -1]]
   :bot/boosts {EXTRA_HAND 0
                FAST_WHEELS 0
                DRILL 0
                X_UNKNOWN_PERK 0}})