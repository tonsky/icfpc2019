(ns icfpc.level
  (:require [icfpc.core :refer :all]
            [icfpc.parser :refer :all]))

(defn coord->idx [level x y] (+ x (* y (:level/width level))))

(defn get-level [level x y]
  (nth (:level/grid level) (coord->idx level x y)))

(defn set-level [level x y value]
  (update level :level/grid assoc (coord->idx level x y) value))

(defn mark-wrapped
  "Apply wrapped to what bot at current pos touches"
  [{:bot/keys [x y layout] :level/keys [width height grid] :as level}]
  (reduce
    (fn [level [dx dy]]
      (let [x' (+ x dx) y' (+ y dy)]
        (if (or (neg? x') (neg? y') (>= x' width) (>= y' height))
          level
          (set-level level x' y' WRAPPED))))
    level
    (:bot/layout level)))

(def prob-001
  {:level/width  8
   :level/height 3
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

(defn bounds [points]
  (let [xs (map first points)
        ys (map second points)]
    {:min-x (apply min xs)
     :max-x (apply max xs)
     :min-y (apply min ys)
     :max-y (apply max ys)}))

(defn load-level-1 [name]
  (let [{:keys [start level obstacles busters]} (parse-level name)]
    (apply min (map first level))))

(comment
  (def level (:level (parse-level "prob-150.desc")))
  (bounds level)
  )