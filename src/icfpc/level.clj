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

(def score-point {EMPTY       1
                  OBSTACLE    0
                  WRAPPED     0
                  EXTRA_HAND  1
                  FAST_WHEELS 1
                  DRILL       1
                  X_UNKNOWN_PERK 1})

(defn position-score [{:bot/keys [x y layout] :level/keys [width height grid] :as level}]
  (reduce
   (fn [score [x y]]
     (+ score (score-point (get-level level x y))))
   0
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

(defn direction [[from-x from-y] [to-x to-y]]
  (cond
    (and (= from-y to-y) (< from-x to-x))
    :right

    (and (= from-x to-x) (< from-y to-y))
    :up

    (and (= from-y to-y) (< to-x from-x))
    :left

    (and (= from-x to-x) (< to-y from-y))
    :down))

(defn fill-line [level [[from-x from-y] [to-x to-y]] value]
  (cond
    ;; left to right
    (and (= from-y to-y) (< from-x to-x))
    (reduce (fn [level x]
              (set-level level x from-y value))
            level
            (range from-x to-x))

    ;; botom up
    (and (= from-x to-x) (< from-y to-y))
    (reduce (fn [level y]
              (set-level level (dec from-x) y value))
            level
            (range from-y to-y))

    ;; right to left
    (and (= from-y to-y) (< to-x from-x))
    (reduce (fn [level x]
              (set-level level x (dec from-y) value))
            level
            (range to-x from-x))

    ;; top down
    (and (= from-x to-x) (< to-y from-y))
    (reduce (fn [level y]
              (set-level level from-x y value))
            level
            (range to-y from-y))))

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
  (let [level (reduce (fn [level [from to]]
                        (fill-line level [from to] value))
                      level
                      (partition 2 1 (into corners (take 1 corners))))]
    level
    (reduce (fn [level [[from-x from-y :as from] [mid-x mid-y :as mid] [to-x to-y :as to]]]
              (let [dir1 (direction from mid)
                    dir2 (direction mid to)]
                (case [dir1 dir2]
                  [:right :down]
;                  (set-level level mid-x mid-y \A)
                  (fill-dfs level [mid-x mid-y] value)

                  [:up :right]
;                  (set-level level (dec mid-x) mid-y \B)
                  (fill-dfs level [(dec mid-x) mid-y] value)

                  [:left :up]
;                  (set-level level (dec mid-x) (dec mid-y) \C)
                  (fill-dfs level [(dec mid-x) (dec mid-y)] value)

                  [:down :left]
;                  (set-level level mid-x (dec mid-y) \D)
                  (fill-dfs level [mid-x (dec mid-y)] value)

                  level)))
            level
            (partition 3 1 (into corners (take 2 corners))))))

(defn load-level [name]
  (let [{:keys [bot-point corners obstacles busters]} (parse-level name)
        [width height] (bounds corners)
        init-level {:level/name name
                    :level/busters busters
                    :level/width width
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

(def lvls (mapv (fn [n]
                  (load-level (format "prob-%03d.desc" n)))
                (range 1 51)))

(comment
  (doseq [lvl lvls]
    (icfpc.bot/print-level lvl))

  *e

  (:corners (parse-level "prob-002.desc"))
  (icfpc.bot/print-level (load-level "prob-011.desc"))


  )