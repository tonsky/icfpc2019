(ns icfpc.bot
  (:require
   [clojure.string :as str]
   [icfpc.core :refer :all]))

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

(defn walk [level shape x y path]
  (if (empty? path)
    level
    (let [[x' y'] (case (first path)
                    \W [x (+ y 1)]
                    \S [x (- y 1)]
                    \A [(- x 1) y]
                    \D [(+ x 1) y])]
      (recur
        (mark-level level x' y' shape)
        shape
        x' y' (next path)))))

(defn make-move-impl [queue covered level]
  (let [[x y path] (peek queue)]
    (cond
      (empty? queue)
      nil

      (or (neg? x) (neg? y) (>= x width) (>= y height)
        (contains? covered [x y]))
      (recur (pop queue) covered level)

      (= E (at-coord level x y))
      [x y path]

      :else
      (recur
        (into queue 
          [[x (+ y 1) (conj path \W)]
           [(- x 1) y (conj path \A)]
           [(+ x 1) y (conj path \D)]
           [x (- y 1) (conj path \S)]])
        (conj covered [x y]) level))))

(defn make-move [x y level]
  (make-move-impl (queue [x y []]) #{} level))

(defn queue [& xs]
  (into clojure.lang.PersistentQueue/EMPTY xs))

(defn print-level [level]
  (doseq [y (range 0 height)]
    (doseq [x (range 0 width)]
      (print (at-coord level x y)))
    (println))
  (println))

(comment
  (loop [x 0
         y 0
         path []
         level (mark-level @*level 0 0 shape)]
    (if-some [[x' y' path'] (make-move x y level)]
      (let [level' (walk level shape x y path')]
        (println (str "[" x "," y "] -> [" x' "," y "] via " (str/join path')))
        (print-level level')
        (recur x' y' (into path path') level'))
      (println "DONE")))
)