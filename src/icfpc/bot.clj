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

(defn score-bot [x y layout level]
  (reduce + 0
    (->> (bot-covering x y layout level)
      (map (fn [[x y]] (score-point x y level))))))

(defn add-extra-hand [level]
  (let [max-x (max-key first (:bot/layout level))]
    (update level :bot/layout conj [(inc max-x) 0])))

(defn fast-wheel-on [level]
  (update level :bot/active-boosters assoc FAST_WHEELS 50))

(defn drill-on [level]
  (update level :bot/active-boosters assoc DRILL 30))

(defn activate-booster [level booster]
  (condp = booster
    EXTRA_HAND (add-extra-hand level)
    FAST_WHEELS (fast-wheel-on level)
    DRILL (drill-on level)))

(defn has-available-booster [level booster]
  (let [v (get (:bot/boosters level) booster)]
    (and (some? v)
         (< 0 v))))

(defn update-boosters [active-boosters]
  (into {}
        (map (fn [[booster ttl :as tpl]]
               (if (= booster EXTRA_HAND)
                 tpl
                 (when (< 0 ttl)
                   [booster (dec ttl)]))))
        active-boosters))

(defn move [level dx dy]
  (let [FW? (is-booster-active level FAST_WHEELS)]
    (-> level
        (update :bot/x + (cond-> dx FW? (* 2)))
        (update :bot/y + (cond-> dy FW? (* 2)))
        ;; todo smth with fast-wheels -- quiclky passed by nodes won't be colored
        (mark-wrapped)
        (update :bot/active-boosters update-boosters))))

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
            (for [[level' path'] (into [[(move level  0  1) (conj path UP)]
                                        [(move level -1  0) (conj path DOWN)]
                                        [(move level  1  0) (conj path RIGHT)]
                                        [(move level  0 -1) (conj path LEFT)]]
                                       (filter some?)
                                       ;; we can always add additional hand, but no need to activate
                                       ;; other boosters if we already have active one
                                       [(when (has-available-booster level EXTRA_HAND)
                                              [(activate-booster level EXTRA_HAND) (conj path EXTRA_HAND)])
                                        (when (and (has-available-booster level FAST_WHEELS)
                                                   (not (is-booster-active level FAST_WHEELS)))
                                              [(activate-booster level FAST_WHEELS) (conj path FAST_WHEELS)])
                                        (when (and (has-available-booster level DRILL)
                                                   (is-booster-active level DRILL))
                                              [(activate-booster level DRILL) (conj path DRILL)])])
                  :when (valid? level')
                  :when (not (contains? covered [(:bot/x level') (:bot/y level')]))]
              [level' path'])
            (sort-by (fn [[level' path']]
                       (cond-> (score-bot (:bot/x level') (:bot/y level') (:bot/layout level') orig-level)
                         ;; score intermediate values if fast wheels are on
                         (is-booster-active orig-level FAST_WHEELS)
                         (+ (score-bot (- (:bot/x level') ))))))
            (reverse)))
        (conj covered [x y])
        orig-level))))

(defn make-move [level]
  (make-move-impl (queue [level []]) #{} level))

(defn print-level [{:level/keys [width height] :as level}]
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

*e