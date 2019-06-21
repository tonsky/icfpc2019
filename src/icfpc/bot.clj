(ns icfpc.bot
  (:require
   [icfpc.core :refer :all]))

(def *level
  (atom
    [EMPTY EMPTY EMPTY EMPTY EMPTY OBSTACLE OBSTACLE
     EMPTY EMPTY EMPTY EMPTY EMPTY EMPTY EMPTY
     EMPTY EMPTY EMPTY EMPTY EMPTY OBSTACLE OBSTACLE]))

(def start [0 0])
(def shape [[0 0] [1 0] [1 1] [1 -1]])
(def boosts {EXTRA_HAND 0
             FAST_WHEELS 0
             DRILL 0
             X_UNKNOWN_PERK 0})

(def width 8)
(def height 3)

(defn coord->idx [x y] (+ x (* y width)))

(defn at-coord [level x y]
  (nth level (coord->idx x y)))

(defn mark-level
  ([level x y]
    (update level (coord->idx x y) assoc WRAPPED))
  ([level x y shape]
    (reduce (fn [level [dx dy]] (mark-level level (+ x dx) (+ y dy))) level shape)))


;; movement

(defn- shift-point [[x y] dir]
  (case dir
    :up [x (inc y)]
    :down [x (dec y)]
    :right [(inc x) y]
    :left [(dec x) y]))

(defn- valid-moves [{:keys [grid w h] :as level}
                    {:keys [x y layout boosts] :as bot}]
  (filterv
   (fn [dir]
     (let [[x y] (shift-point [x y] dir)]
       (and (<= 0 x)
            (<= 0 y)
            (< x w)
            (< y h)
            (not= (at-coord grid x y)
                  OBSTACLE))))
   [:up :down :left :right]))

(defn- shape->abs [x y layout]
  (mapv
   (fn [[x' y']]
     [(+ x x')
      (+ y y')])
   layout))

(defn get-score [x y {:keys [grid w h] :as level}]
  (if (or (< x 0)
          (< y 0)
          (<= w x)
          (<= h y))
    0
    (let [v (at-coord grid x y)]
      (case v
        EMPTY 1
        OBSTACLE 0
        WRAPPED 0
        EXTRA_HAND 1
        FAST_WHEELS 1
        DRILL 1
        X_UNKNOWN_PERK 1))))

(defn next-move [{:keys [grid w h] :as level}
                 {:keys [x y layout boosts] :as bot}]
  (max-key
   (fn [dir]
     (let [drone-shifted (mapv
                          (shift-point dir)
                          (shape->abs x y layout))]
       (apply +
              (map
                (fn [[x y]]
                     (get-score x y level))
               drone-shifted))))
   (valid-moves level bot)))