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
(s/def :bot/collected-boosters
       (s/map-of #{EXTRA_HAND FAST_WHEELS DRILL X_UNKNOWN_PERK} :boosters/amount))
(s/def :bot/active-boosters (s/map-of #{EXTRA_HAND FAST_WHEELS DRILL X_UNKNOWN_PERK} :boosters/ttl))
(s/def :bot/layout vector?)
(s/def :bot/x nat-int?)
(s/def :bot/y nat-int?)

(s/def :frame/frame
       (s/keys :level/width
               :level/height
               :level/grid

               :bot/collected-boosters
               :bot/active-boosters
               :bot/layout
               :bot/x
               :bot/y))

(defn add-extra-hand [level]
  (when (pos? (get (:bot/collected-boosters level) EXTRA_HAND 0))
    (let [variants (clojure.set/difference #{[0 1] [0 -1] [-1 0] [1 0] [1 1] [-1 -1] [-1 1] [1 -1]}
                                           (:bot/layout level))
          [x y :as p] (first variants) ;; square around the bot
;           [x y] (apply max-key first (:bot/layout level))
          ]
      (when (some? p)
        (-> level
            (update :bot/layout conj [(inc x) y])
            (update-in [:bot/collected-boosters EXTRA_HAND] dec))))))

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
  (let [v (get (:bot/collected-boosters level) booster)]
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
        (update :bot/active-boosters update-boosters))))

(defn rotate-ccw [level]
  (update level :bot/layout
    (fn [layout]
      (mapv (fn [[dx dy]] [(- dy) dx]) layout))))

(defn rotate-cw [level]
  (update level :bot/layout
    (fn [layout]
      (mapv (fn [[dx dy]] [dy (- dx)]) layout))))

(def ^:dynamic *max-path-len*)

(defn act [level action]
  (condp = action
    EXTRA_HAND (add-extra-hand level)
    UP         (move level 0 1)
    DOWN       (move level 0 -1)
    LEFT       (move level -1 0)
    RIGHT      (move level 1 0)
    ROTATE_CW  (rotate-cw level)
    ROTATE_CCW (rotate-ccw level)))

(def counter
  {LEFT RIGHT
   RIGHT LEFT
   UP DOWN
   DOWN UP
   ROTATE_CW ROTATE_CCW
   ROTATE_CCW ROTATE_CW})

(defn lookahead-score [[_ path score]]
  (bit-or
    (bit-shift-left score 10)
    (- 1024 (count path))))

(defn lookahead-impl [queue paths]
  (let [[level path score] (peek queue)
        {:level/keys [width height] :bot/keys [x y]} level]
    (cond
      (empty? queue)
      (when-not (empty? paths)
        (let [[level path score] (apply max-key lookahead-score paths)]
          [level path]))

      (>= (count path) *max-path-len*)
      (recur
        (pop queue)
        (if (pos? score)
          (conj paths [level path score])
          paths))

      :else
      (let [last-action (last path)
            moves (for [action [EXTRA_HAND ROTATE_CW ROTATE_CCW RIGHT LEFT UP DOWN]
                        :when  (not= last-action (counter action))
                        :let   [level' (act level action)]
                        :when  (some? level')
                        :when  (valid? level')
                        :let   [path'  (conj path action)
                                booster (get (:level/boosters level) [(:bot/x level') (:bot/y level)])
                                dscore (cond
                                         (= action EXTRA_HAND)
                                         1000
                                         (some? booster)
                                         100
                                         :else
                                         (position-score level' path'))]
                        :when  (pos? dscore)]
                    [(mark-wrapped level') path' (+ score dscore)])]
        (recur
          (into (pop queue) moves)
          (if (pos? score)
            (conj paths [level path score])
            paths))))))

(defn lookahead [level]
  (lookahead-impl (queue [level [] 0]) #{}))

(defn make-move-impl [queue seen]
  (let [[{:level/keys [width height]
          :bot/keys [x y] :as level} path score] (peek queue)]
    (cond
      (empty? queue)
      nil

      (< 0 score)
      [level path score]

      :else
      (let [moves (->>
                   (for [[level' path'] (into [[(move level  0  1) (conj path UP)]
                                               [(move level  0 -1) (conj path DOWN)]
                                               [(move level  1  0) (conj path RIGHT)]
                                               [(move level -1  0) (conj path LEFT)]
                                               [(rotate-cw level)  (conj path ROTATE_CW)]
                                               [(rotate-ccw level) (conj path ROTATE_CCW)]]
                                              ; (filter some?)
                                              ;; we can always add additional hand, but no need to activate
                                              ;; other boosters if we already have active one
                                              []
                                              #_[(when (has-available-booster level EXTRA_HAND)
                                                     [(activate-booster level EXTRA_HAND) (conj path EXTRA_HAND)])
                                               (when (and (has-available-booster level FAST_WHEELS)
                                                          (not (is-booster-active level FAST_WHEELS)))
                                                     [(activate-booster level FAST_WHEELS) (conj path FAST_WHEELS)])
                                               (when (and (has-available-booster level DRILL)
                                                          (is-booster-active level DRILL))
                                                     [(activate-booster level DRILL) (conj path DRILL)])])
                         :when (valid? level')
                         :when (not (contains? seen [(:bot/x level') (:bot/y level')]))]
                     [(mark-wrapped level') path' (+ score (position-score level' path'))])
                   (sort-by (fn [[_ _ score]] score))
                   (reverse))]
        (recur
          (into (pop queue) moves)
          (into seen (map (fn [[level' _ _]] [(:bot/x level') (:bot/y level')]) moves)))))))

(defn make-move [level]
  (make-move-impl (queue [level [] 0 ]) #{[(:bot/x level) (:bot/y level)]}))

(defn print-level [{:level/keys [width height name boosters] :as level} & {:keys [colored?] :or {colored? true}}]
  (println name)
  (doseq [y (range (dec height) -1 -1)]
    (doseq [x (range 0 width)
            :let [v (get-level level x y)
                  booster (get boosters [x y])]]
        (cond
          (and (= x (:bot/x level)) (= y (:bot/y level)))
          (if colored?
            (print "\033[37;1;41m☺\033[0m")
            (print "☺"))

          (some? booster)
          (if colored?
            (print (str "\033[42m" booster "\033[0m"))
            (print booster))

          (= v EMPTY)
          (if colored?
            (print "\033[103m•\033[0m")
            (print "•"))

          (= v WRAPPED)
          (if colored?
            (print "\033[43m+\033[0m")
            (print "+"))

          :else
          (print (get-level level x y))))
    (println))
  (println))

(defn solve [level & [{:keys [debug? delay lookahead? max-path-len]
                       :or {lookahead? true, debug? true, delay 50, max-path-len 3}}]]
  (let [t0 (System/currentTimeMillis)]
    (binding [*max-path-len* max-path-len]
      (loop [path  []
             level (mark-wrapped level)]
        (if-some [[level' path'] (or
                                   (when lookahead? (lookahead level))
                                   (make-move level))]
          (do
            (when debug?
              (println "\033[2J")
              (prn "Boosters: " (:bot/collected-boosters level))
              (println (str "[" (:bot/x level) "," (:bot/y level) "] -> [" (:bot/x level') "," (:bot/y level') "] via " (str/join path')))
              (print-level level')
              (println (count (into path path')) "via" (str/join (into path path')))
              (Thread/sleep delay))
            (when-not (Thread/interrupted)
              (recur (into path path') level')))
          {:path  (str/join path)
           :score (count path)
           :time  (- (System/currentTimeMillis) t0)})))))

(defn show-boosters [{:level/keys [boosters] :as level}]
  (let [level' (reduce (fn [level [[x y] kind]]
                         (set-level level x y kind))
                       level
                       boosters)]
    (print-level level' :colored? false)))

(comment
  (print-level (load-level "prob-001.desc") :colored? false)
  (show-boosters (load-level "prob-050.desc"))
  *e

  (solve prob-001 true)
)