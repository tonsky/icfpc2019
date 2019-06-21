(ns icfpc.bot
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as str]
   [icfpc.core :refer :all]
   [icfpc.level :refer :all]))

(s/def :level/width nat-int?)
(s/def :level/height nat-int?)
(s/def :level/grid (s/coll-of #{EMPTY OBSTACLE WRAPPED EXTRA_HAND FAST_WHEELS DRILL X_UNKNOWN_PERK} :kind vector?))
(s/def :boosters/amount nat-int?)
(s/def :boosters/ttl nat-int?)
(s/def :bot/boosters (s/map-of #{EXTRA_HAND FAST_WHEELS DRILL X_UNKNOWN_PERK} :boosters/amount))
(s/def :bot/active-boosters (s/map-of #{EXTRA_HAND FAST_WHEELS DRILL X_UNKNOWN_PERK} :boosters/ttl))
(s/def :bot/layout vector?)
(s/def :bot/x nat-int?)
(s/def :bot/y nat-int?)

(s/def :frame/frame
       (s/keys :level/width
               :level/height
               :level/grid

               :bot/boosters
               :bot/active-boosters
               :bot/layout
               :bot/x
               :bot/y))

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

(defn make-move-impl [queue seen orig-level]
  (let [[{:level/keys [width height]
          :bot/keys [x y] :as level} path] (peek queue)]
    (cond
      (empty? queue)
      nil

      (= EMPTY (get-level orig-level x y))
      [level path]

      :else
      (let [moves (->>
                    (for [[level' path'] [[(move level  0  1) (conj path UP)]
                                          [(move level -1  0) (conj path DOWN)]
                                          [(move level  1  0) (conj path RIGHT)]
                                          [(move level  0 -1) (conj path LEFT)]]
                          :when (valid? level')
                          :when (not (contains? seen [(:bot/x level') (:bot/y level')]))]
                      [level' path'])
                    (sort-by (fn [[level' path']] (score-bot level' orig-level)))
                    (reverse))]
        (recur
          (into (pop queue) moves)
          (into seen (map (fn [[level' path']] [(:bot/x level') (:bot/y level')]) moves))
          orig-level)))))

(defn make-move [level]
  (make-move-impl (queue [level []]) #{[(:bot/x level) (:bot/y level)]} level))

(defn print-level [{:level/keys [width height name] :as level}]
  (println name)
  (doseq [y (range (dec height) -1 -1)]
    (doseq [x (range 0 width)
            :let [v (get-level level x y)]]
        (cond
          (and (= x (:bot/x level)) (= y (:bot/y level)))
          (print "\033[37;1;41m☺\033[0m")

          (= v EMPTY)
          (print "\033[103m•\033[0m")

          (= v WRAPPED)
          (print "\033[43m+\033[0m")

          :else
          (print (get-level level x y))))
    (println))
  (println))

(defn solve [level & [{:keys [debug? delay] :or {debug? true delay 50}}]]
  (loop [path  []
         level (mark-wrapped level)]
    (if-some [[level' path'] (make-move level)]
      (do
        (when debug?
          (println "\033[2J")
          (println (str "[" (:bot/x level) "," (:bot/y level) "] -> [" (:bot/x level') "," (:bot/y level') "] via " (str/join path')))
          (print-level level')
          (when (some? delay)
            (Thread/sleep delay)))
        (recur (into path path') level'))
      (str/join path))))

(comment
  (solve prob-001 true)
)