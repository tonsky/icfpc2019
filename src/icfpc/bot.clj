(ns icfpc.bot
  (:require
   [clojure.string :as str]
   [icfpc.core :refer :all]
   [icfpc.level :refer :all]))

(defn score-point [x y {:level/keys [width height] :as level}]
  (get {EMPTY       1
        OBSTACLE    0
        WRAPPED     0
        EXTRA_HAND  1
        FAST_WHEELS 1
        DRILL       1
        X_UNKNOWN_PERK 1}
    (get-level level x y)))

(defn score-bot [position level]
  (reduce + 0
    (->> (bot-covering (:bot/x position) (:bot/y position) position)
      (map (fn [[x y]] (score-point x y level))))))

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

      (= EMPTY (get-level orig-level x y))
      [level path]

      :else
      (recur
        (into (pop queue)
          (->>
            (for [[level' path'] [[(move level  0  1) (conj path UP)]
                                  [(move level -1  0) (conj path DOWN)]
                                  [(move level  1  0) (conj path RIGHT)]
                                  [(move level  0 -1) (conj path LEFT)]]
                  :when (valid? level')
                  :when (not (contains? covered [(:bot/x level') (:bot/y level')]))]
              [level' path'])
            (sort-by (fn [[level' path']] (score-bot level' orig-level)))
            (reverse)))
        (conj covered [x y])
        orig-level))))

(defn make-move [level]
  (make-move-impl (queue [level []]) #{} level))

(defn print-level [{:level/keys [width height name] :as level}]
  (println name)
  (doseq [y (range (dec height) -1 -1)]
    (doseq [x (range 0 width)]
      (if (and (= x (:bot/x level)) (= y (:bot/y level)))
        (print "☺")
        (print (get-level level x y))))
    (println))
  (println))

(defn solve [level debug?]
  (loop [path  []
         level (mark-wrapped level)]
    (if-some [[level' path'] (make-move level)]
      (do
        (when debug?
          (println (str "[" (:bot/x level) "," (:bot/y level) "] -> [" (:bot/x level') "," (:bot/y level') "] via " (str/join path')))
          (print-level level'))
        (recur (into path path') level'))
      (str/join path))))

(comment
  (solve prob-001 true)
)