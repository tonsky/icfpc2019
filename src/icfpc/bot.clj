(ns icfpc.bot
  (:require
   [clojure.string :as str]
   [icfpc.core :refer :all]
   [icfpc.level :refer :all]))

;; movement

(defn valid? [{:keys [bot/x bot/y level/width level/height]}]
  (and (< -1 x width) (< -1 y height)))

(defn- shift-point [[x y] dir]
  (case dir
    UP    [x (inc y)]
    DOWN  [x (dec y)]
    RIGHT [(inc x) y]
    LEFT  [(dec x) y]))

(defn- valid-moves [{:bot/keys [x y layout boosts]
                     :level/keys [grid width height] :as level}]
  (filterv
   (fn [dir]
     (let [[x y] (shift-point [x y] dir)]
       (and (<= 0 x)
            (<= 0 y)
            (< x width)
            (< y height)
            (not= (get-level level x y)
                  OBSTACLE))))
   [UP DOWN LEFT RIGHT]))

(defn- shape->abs [x y layout]
  (mapv
   (fn [[x' y']]
     [(+ x x')
      (+ y y')])
   layout))

(defn- get-score [x y {:level/keys [grid width height] :as level}]
  (if (or (< x 0)
          (< y 0)
          (<= width x)
          (<= height y))
    0
    (let [v (get-level grid x y)]
      (case v
        EMPTY 1
        OBSTACLE 0
        WRAPPED 0
        EXTRA_HAND 1
        FAST_WHEELS 1
        DRILL 1
        X_UNKNOWN_PERK 1))))

(defn next-move [{:bot/keys [x y layout boosts] :as level}]
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
   (valid-moves level)))

(defn move [level dx dy]
  (-> level
    (update :bot/x + dx)
    (update :bot/y + dy)
    (mark-wrapped)))

(defn make-move-impl [queue covered orig-level]
  (let [[{:level/keys [width height]
          :bot/keys [x y] :as level} path] (peek queue)]
    (cond
      (empty? queue)
      nil

      (or (not (valid? level))
          (valid-point? covered [x y]))
      (recur (pop queue) covered orig-level)

      (= EMPTY (get-level orig-level x y))
      [level path]

      :else
      (recur
        (into queue 
          [[(move level  0  1) (conj path UP)]
           [(move level -1  0) (conj path DOWN)]
           [(move level  1  0) (conj path RIGHT)]
           [(move level  0 -1) (conj path LEFT)]])
        (conj covered [x y])
        orig-level))))

(defn make-move [level]
  (make-move-impl (queue [level []]) #{} level))

(defn print-level [{:level/keys [width height] :as level}]
  (doseq [y (range (dec height) -1 -1)]
    (doseq [x (range 0 width)]
      (if (and (= x (:bot/x level)) (= y (:bot/y level)))
        (print "!")
        (print (get-level level x y))))
    (println))
  (println))

(defn solve [level]
  (loop [path  []
         level (mark-wrapped level)]
    (if-some [[level' path'] (make-move level)]
      (do
        (println (str "[" (:bot/x level) "," (:bot/y level) "] -> [" (:bot/x level') "," (:bot/y level') "] via " (str/join path')))
        (print-level level')
        (recur (into path path') level'))
      (println "DONE" (str/join path)))))

(comment
  (solve prob-001)
)