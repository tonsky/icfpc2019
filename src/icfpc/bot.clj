(ns icfpc.bot
  (:require
   [icfpc.core :refer :all]))

(def *level
  (atom
    [E E E E E V V
     E E E E E E E
     E E E E E V V]))

(def start [0 0])
(def shape [[0 0] [1 0] [1 1] [1 -1]])
(def boosts {B 0 F 0 L 0 X 0})

(def width 8)
(def height 3)

(defn coord->idx [x y] (+ x (* y width)))

(defn at-coord [level x y]
  (nth level (coord->idx x y)))

(defn mark-level
  ([level x y]
    (update level (coord->idx x y) assoc W))
  ([level x y shape]
    (reduce (fn [level [dx dy]] (mark-level level (+ x dx) (+ y dy))) level shape)))