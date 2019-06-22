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
          [x y :as p] (first variants)]
      (when (some? p)
        (-> level
            (update :bot/layout conj [x y])
            (update-in [:bot/collected-boosters EXTRA_HAND] dec)
            (update :score + 1000)
            (update :path str "B(" x "," y ")"))))))

(defn move [level dx dy action]
  (some-> level
    (update :bot/x + dx)
    (update :bot/y + dy)
    (valid?)
    (mark-wrapped)
    (update :path str action)))

(defn rotate-ccw [level]
  (-> level
    (update :bot/layout
      (fn [layout]
        (mapv (fn [[dx dy]] [(- dy) dx]) layout)))
    (mark-wrapped)
    (update :path str ROTATE_CCW)))

(defn rotate-cw [level]
  (-> level
    (update :bot/layout
      (fn [layout]
        (mapv (fn [[dx dy]] [dy (- dx)]) layout)))
    (mark-wrapped)
    (update :path str ROTATE_CW)))

(def ^:dynamic *max-path-len*)

(defn act [level action]
  ; (prn "act" level (:bot/x level) (:bot/y level) action)
  (condp = action
    EXTRA_HAND (add-extra-hand level)
    UP         (move level 0 1 UP)
    DOWN       (move level 0 -1 DOWN)
    LEFT       (move level -1 0 LEFT)
    RIGHT      (move level 1 0 RIGHT)
    ROTATE_CW  (rotate-cw level)
    ROTATE_CCW (rotate-ccw level)))

(def counter
  {LEFT RIGHT
   RIGHT LEFT
   UP DOWN
   DOWN UP
   ROTATE_CW ROTATE_CCW
   ROTATE_CCW ROTATE_CW})

(defn lookahead-score [level]
  [(:score level) (- (count (:path level)))])

(defn lookahead [level]
  (let [max-len (+ (count (:path level)) *max-path-len*)]
    (loop [queue  (queue level)
           levels #{}]
      (let [level (peek queue)
            {:level/keys [width height] :bot/keys [x y] :keys [path]} level]
        (cond
          (empty? queue)
          (when-not (empty? levels)
            (max-by lookahead-score levels))

          (>= (count path) max-len)
          (recur (pop queue) (conj levels level))

          :else
          (let [last-action (last path)
                moves (for [action [EXTRA_HAND ROTATE_CW ROTATE_CCW RIGHT LEFT UP DOWN]
                            :when  (not= last-action (counter action))
                            :let   [level' (act level action)]
                            :when  (some? level')
                            :when  (> (:score level') (:score level))]
                        level')]
            (recur (into (pop queue) moves) levels)))))))

(defn make-move [level]
  (let [min-score (:score level)]
    (loop [queue (queue level)
           seen  #{[(:bot/x level) (:bot/y level)]}]
      (let [level (peek queue)
            {:level/keys [width height] :bot/keys [x y] :keys [path score]} level]
        (cond
          (empty? queue)
          nil

          (> score min-score)
          level

          :else
          (let [moves (->>
                        (for [action [RIGHT LEFT UP DOWN]
                              :let   [level' (act level action)]
                              :when  (some? level')
                              :when  (not (contains? seen [(:bot/x level') (:bot/y level')]))]
                          level')
                        (sort-by :score)
                        (reverse))]
            (recur
              (into (pop queue) moves)
              (into seen (map (fn [l] [(:bot/x l) (:bot/y l)]) moves)))))))))

(defn print-level [{:level/keys [width height name boosters] :bot/keys [x y] :as level} & {:keys [colored?] :or {colored? true}}]
  (println name)
  (doseq [y (range (min (dec height) (+ y 20)) (dec (max 0 (- y 20))) -1)]
    (doseq [x (range (max 0 (- x 50)) (min width (+ x 50)))
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

(defn solve [level & [{:keys [debug? lookahead? max-path-len]
                       :or {lookahead? true, debug? true, max-path-len 3}}]]
  (let [t0 (System/currentTimeMillis)]
    (binding [*max-path-len* max-path-len]
      (loop [level      (mark-wrapped level)
             last-frame (System/currentTimeMillis)]
        (if-some [level' (or
                           (when lookahead? (lookahead level))
                           (make-move level))]
          (if (and debug? (> (- (System/currentTimeMillis) last-frame) 200))
            (do
              (println "\033[2J")
              (print-level level')
              (println "Boosters: " (:bot/collected-boosters level))
              (println "Layout: " (:bot/layout level))
              (println (str "[" (:bot/x level) "," (:bot/y level) "] -> [" (:bot/x level') "," (:bot/y level') "]"))
              (println (count (:path level')) "via" (:path level'))
              (when-not (Thread/interrupted)
                (recur level' (System/currentTimeMillis))))
            (recur level' last-frame))
          {:path  (:path level)
           :score (count (:path level))
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