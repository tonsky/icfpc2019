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
    [(inc (apply max xs)) (inc (apply max ys))]))

(defn fill-line [level [[from-x from-y] [to-x to-y]] value]
  (cond
    (= from-x to-x)
    (reduce level
            (fn [level y]
              (set-level level from-x y value))
            (range (min from-y to-y) (inc (max from-y to-y))))

    (= from-y to-y)
    (reduce level
            (fn [level x]
              (set-level level x from-y value))
            (range (min from-x to-x) (inc (max from-x to-x))))))

(defn fill-poly [level corners value]
  (let [corners (conj corners (first corners))]
    (reduce fill-line
            level
            (partition 2 1 corners))))

(defn load-level [name]
  (let [{:keys [bot-point corners obstacles busters]} (parse-level name)
        [width height] (bounds corners)
        init-level {:level/width width
                    :level/height height
                    :level/grid (vec (repeat (* width height) EMPTY))
                    :bot/x (first bot-point)
                    :bot/y (second bot-point)
                    :bot/layout [[0 0] [1 0] [1 1] [1 -1]]
                    :bot/boosts {EXTRA_HAND 0
                                 FAST_WHEELS 0
                                 DRILL 0
                                 X_UNKNOWN_PERK 0}}]
    init-level))

(comment
  (partition 2 1 (vec (range 5)))
  (def level (:corners (parse-level "prob-150.desc")))
  (bounds level)
  )