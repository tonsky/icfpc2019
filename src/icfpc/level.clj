(ns icfpc.level
  (:require [icfpc.core :refer :all]
            [icfpc.parser :refer :all]))

(defn coord->idx [level x y] (+ x (* y (:level/width level))))

(defn get-level [level x y]
  (nth (:level/grid level) (coord->idx level x y)))

(defn valid-point? [{:level/keys [width height] :as level} [x y]]
  (and (< -1 x width) (< -1 y height)))

(defn set-level [level x y value]
  (update level :level/grid assoc (coord->idx level x y) value))

(defn is-booster-active [level booster]
  (get (:bot/active-boosters level) booster))

(defn valid?
  ([x y {:level/keys [width height] :as level}]
    (and (< -1 x width)
      (< -1 y height)
      (or
       (is-booster-active level DRILL)
       (not (= OBSTACLE (get-level level x y))))))
  ([{:bot/keys [x y] :as level}]
    (valid? x y level)))

(defn bot-covering [x y layout level]
  (reduce 
    (fn [acc [dx dy]]
      (let [x' (+ x dx)
            y' (+ y dy)]
        (if (valid? x' y' level)
          (conj acc [x' y'])
          acc)))
    []
    layout))

(defn mark-wrapped
  "Apply wrapped to what bot at current pos touches"
  [{:bot/keys [x y layout] :level/keys [width height grid] :as level}]
  (reduce
    (fn [level [x y]]
      (let [before (get-level level x y)]
        (if (= EMPTY before)
          (set-level level x y WRAPPED)
          level)))
    level
    (bot-covering x y layout level)))

(def prob-001
  {:level/width  7
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
    (reduce (fn [level y]
              (set-level level from-x y value))
            level
            (range (min from-y to-y) (inc (max from-y to-y))))

    (= from-y to-y)
    (reduce (fn [level x]
              (set-level level x from-y value))
            level
            (range (min from-x to-x) (inc (max from-x to-x))))))

(defn fill-dfs [level [x y :as current] value]
  (if (and (valid-point? level current) (not= (get-level level x y) value))
    (let [level (set-level level x y value)]
      (-> level
          (fill-dfs [(inc x) y] value)
          (fill-dfs [x (inc y)] value)
          (fill-dfs [(dec x) y] value)
          (fill-dfs [x (dec y)] value)))
    level))

(defn fill-poly [level corners value]
  (let [corners (conj corners (first corners))
        level (reduce (fn [level [from to]]
                        (fill-line level [from to] value))
                      level
                      (partition 2 1 corners))]
    (reduce (fn [level [[from-x from-y] [to-x to-y]]]
              (cond
                (and (= from-y to-y) (< from-x to-x))
                (fill-dfs level [(inc from-x) (inc from-y)] value)

                (and (= from-x to-x) (< from-y to-y))
                (fill-dfs level [(dec from-x) (inc from-y)] value)

                (and (= from-y to-y) (< to-x from-x))
                (fill-dfs level [(dec from-x) (dec from-y)] value)

                (and (= from-x to-x) (< to-y from-y))
                (fill-dfs level [(inc from-x) (dec from-y)] value)))
            level
            (partition 2 1 corners))))

(defn load-level [name]
  (let [{:keys [bot-point corners obstacles busters]} (parse-level name)
        [width height] (bounds corners)
        init-level {:level/width width
                    :level/height height
                    :level/grid (vec (repeat (* width height) OBSTACLE))
                    :bot/x (first bot-point)
                    :bot/y (second bot-point)
                    :bot/layout [[0 0] [1 0] [1 1] [1 -1]]
                    :bot/boosts {EXTRA_HAND 0
                                 FAST_WHEELS 0
                                 DRILL 0
                                 X_UNKNOWN_PERK 0}}]
    (reduce (fn [level obs]
              (fill-poly level obs OBSTACLE))
            (fill-poly init-level corners EMPTY)
            obstacles)))

(comment
  *e

  (:bot-point (parse-level "prob-010.desc"))
  (:bot/x (parse-level "prob-010.desc"))
  (def l (load-level "prob-002.desc"))
  (:bot/x l)


  (icfpc.bot/print-level l)


  *e
  (partition 2 1 (vec (range 5)))
  (def level (:corners (parse-level )))
  (bounds level)
  )