(ns icfpc.bot
  (:require
   [clojure.set :as set]
   [clojure.spec.alpha :as s]
   [clojure.string :as str]
   [icfpc.core :refer :all]
   [icfpc.level :refer :all]))

(s/def :level/width nat-int?)
(s/def :level/height nat-int?)
(s/def :level/grid (s/coll-of #{EMPTY OBSTACLE WRAPPED EXTRA_HAND FAST_WHEELS DRILL SPAWN} :kind vector?))
(s/def :boosters/amount nat-int?)
(s/def :boosters/ttl nat-int?)
(s/def :bot/collected-boosters
       (s/map-of #{EXTRA_HAND FAST_WHEELS DRILL SPAWN} :boosters/amount))
(s/def :bot/active-boosters (s/map-of #{EXTRA_HAND FAST_WHEELS DRILL SPAWN} :boosters/ttl))
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

(defn next-hand [level]
  (let [layout (set (:layout level))]
    (cond
      (contains? layout [1 0]) ;; rigth
      [1 (inc (apply max (map second layout)))]

      (contains? layout [0 1]) ;; up
      [(dec (apply min (map first layout))) 1]

      (contains? layout [-1 0]) ;; left
      [-1 (dec (apply min (map second layout)))]

      (contains? layout [0 -1]) ;; down
      [(inc (apply max (map first layout))) -1])))

(defn add-extra-hand [level]
  (when (pos? (get (:collected-boosters level) EXTRA_HAND 0))
    (let [[x y :as p] (next-hand level)]
      (when (some? p)
        (-> level
            (update :layout conj [x y])
            (spend  :collected-boosters EXTRA_HAND)
            (update :score + 1000)
            (update :path str "B(" x "," y ")"))))))

(defn fast? [level]
  (pos? (get (:active-boosters level) FAST_WHEELS 0)))

(defn add-fast-wheels [level]
  (when (and (pos? (get (:collected-boosters level) FAST_WHEELS 0))
          (not (fast? level)))
    (-> level
      (spend :collected-boosters FAST_WHEELS)
      (assoc-in [:active-boosters FAST_WHEELS] 51)
      (update :score + 1000)
      (update :path str FAST_WHEELS))))

(defn drill? [level]
  (pos? (get (:active-boosters level) DRILL 0)))

(defn add-drill [level]
  (when (and (pos? (get (:collected-boosters level) DRILL 0))
          (not (fast? level)))
    (-> level
      (spend :collected-boosters DRILL)
      (assoc-in [:active-boosters DRILL] 31)
      (update :score + 1000)
      (update :path str DRILL))))

(defn set-beakon [level]
  (when (and (pos? (get (:collected-boosters level) TELEPORT 0))
          (not (contains? (:beakons level) [(:x level) (:y level)]))
          (every?
            (fn [[bx by]]
              (>= (+ (Math/abs (- (:x level) bx))
                     (Math/abs (- (:y level) by)))
                  50))
            (:beakons level)))
    (-> level
      (spend :collected-boosters TELEPORT)
      (update :beakons (fnil conj []) [(:x level) (:y level)])
      (update :score + 1000)
      (update :path str SET_BEAKON))))

(defn extra-move [level dx dy]
  (if (fast? level)
    (let [level' (-> level
                   (update :x + dx)
                   (update :y + dy))]
      (if (valid? level')
        (mark-wrapped level')
        level))
    level))

(defn move [level dx dy action]
  (some-> level
    (update :x + dx)
    (update :y + dy)
    (valid?)
    (mark-wrapped)
    (extra-move dx dy)
    (update :path str action)))

(defn rotate-ccw [level]
  (-> level
    (update :layout
      (fn [layout]
        (mapv (fn [[dx dy]] [(- dy) dx]) layout)))
    (mark-wrapped)
    (update :path str ROTATE_CCW)))

(defn rotate-cw [level]
  (-> level
    (update :layout
      (fn [layout]
        (mapv (fn [[dx dy]] [dy (- dx)]) layout)))
    (mark-wrapped)
    (update :path str ROTATE_CW)))

(defn jump [level idx]
  (when-some [[bx by] (nth (:beakons level) idx nil)]
    (when (not= [(:x level) (:y level)] [bx by])
      (-> level
        (assoc :x bx :y by)
        (mark-wrapped)
        (update :path str JUMP "(" bx "," by ")")))))

(def ^:dynamic *max-path-len*)
(def ^:dynamic *disabled*)

(defn act [level action]
  (condp = action
    UP          (move level 0 1 UP)
    DOWN        (move level 0 -1 DOWN)
    LEFT        (move level -1 0 LEFT)
    RIGHT       (move level 1 0 RIGHT)
    ROTATE_CW   (rotate-cw level)
    ROTATE_CCW  (rotate-ccw level)
    :jump0      (jump level 0)
    :jump1      (jump level 1)
    :jump2      (jump level 2)
    EXTRA_HAND  (add-extra-hand level)
    FAST_WHEELS (add-fast-wheels level) 
    DRILL       (add-drill level)
    SET_BEAKON  (set-beakon level)
    WAIT        (update level :path str WAIT)))

(def counter
  {LEFT RIGHT
   RIGHT LEFT
   UP DOWN
   DOWN UP
   ROTATE_CW ROTATE_CCW
   ROTATE_CCW ROTATE_CW})

(defn lookahead-score [level]
  [(:score level) (- (count (:path level)))])

(defn lookahead [base-level]
  (let [max-len (+ (count (:path base-level)) *max-path-len*)
        queue   (java.util.ArrayDeque. [base-level])]
    (loop [max-level nil
           max-score nil]
      (if-some [level (.poll queue)]
        (let [{:keys [path]} level
              score (lookahead-score level)]
          (when (< (count path) max-len)
            (let [last-action (last path)]
              (doseq [action [EXTRA_HAND DRILL FAST_WHEELS SET_BEAKON ROTATE_CW ROTATE_CCW RIGHT LEFT UP DOWN]
                      :when  (not (contains? *disabled* action))
                      :when  (not= last-action (counter action))
                      :let   [level' (act level action)]
                      :when  (some? level')
                      :when  (> (:score level') (:score level))]
                (.add queue (wear-off-boosters level')))))
          (cond+
            (identical? base-level level)    (recur max-level max-score)
            (nil? max-level)                 (recur level score)
            (pos? (compare score max-score)) (recur level score)
            :else                            (recur max-level max-score)))
          max-level))))

(defn make-move [level]
  (let [min-score (:score level)
        seen      (java.util.HashSet. [(:x level) (:y level)])
        queue     (java.util.ArrayDeque. [level])]
    (loop []
      (when-some [{:keys [x y] :as level} (.poll queue)]
        (if-some [level' (or
                           (add-extra-hand level)
                           (add-drill level)
                           (add-fast-wheels level)
                           (set-beakon level))]
          (do
            (.add queue
              (-> level'
                (assoc :score min-score)
                (wear-off-boosters)))
            (recur))
          (let [moves (for [action [:jump0 :jump1 :jump2 RIGHT LEFT UP DOWN]
                            :when  (not (contains? *disabled* action))
                            :let   [level' (act level action)]
                            :when  (some? level')
                            :when  (not (.contains seen [(:x level') (:y level')]))]
                        level')]
            (cond+
              (empty? moves)
              (do
                (.add queue (-> level (act WAIT) (wear-off-boosters)))
                (recur))

              :let [the-move (seek #(or (== 0 (:empty %)) (> (:score %) min-score)) moves)]

              (some? the-move)
              (wear-off-boosters the-move)

              :else
              (do
                (doseq [move moves]
                  (.add seen [(:x move) (:y move)])
                  (.add queue (wear-off-boosters move)))
                (recur)))))))))

(defn print-level [{:keys [width height name boosters x y] :as level} 
                   & {:keys [colored? max-w max-h] :or {max-w 50 max-h 30 colored? true}}]
  (println name)
  (doseq [y (range (min (dec height) (+ y max-h)) (dec (max 0 (- y max-h))) -1)]
    (doseq [x (range (max 0 (- x max-w)) (min width (+ x max-w)))
            :let [v (get-level level x y)
                  booster (get boosters [x y])]]
        (cond
          (and (= x (:x level)) (= y (:y level)))
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

(defn path-score [path]
  (count (re-seq #"[A-Z]" path)))

(defn solve [level & [{:keys [debug? lookahead? max-path-len delay disabled]
                       :or {lookahead? true, debug? true, max-path-len 2, disabled #{}}}]]
  (let [t0 (System/currentTimeMillis)]
    (binding [*max-path-len* max-path-len
              *disabled*     disabled]
      (loop [level      (mark-wrapped level)
             last-frame (System/currentTimeMillis)]
        (if-some [level' (when (pos? (:empty level))
                           (or
                             (when lookahead? (lookahead level))
                             (make-move level)))]
          (if (and debug?
                (or (some? delay)
                  (> (- (System/currentTimeMillis) last-frame) 200)))
            (do
              (println "\033[2J")
              (print-level level')
              (println "Active:" (filter #(pos? (second %)) (:active-boosters level)) "collected:" (:collected-boosters level))
              (println "Hands:" (dec (count (:layout level))) "layout:" (:layout level))
              (println "Beakons:" (:beakons level))
              (println "Score:" (path-score (:path level')) #_#_"via" (:path level'))
              (when (some? delay)
                (Thread/sleep delay))
              (when-not (Thread/interrupted)
                (recur level' (System/currentTimeMillis))))
            (recur level' last-frame))
          (let [res   {:path  (:path level)
                       :empty (:empty level)
                       :score (path-score (:path level))
                       :time  (- (System/currentTimeMillis) t0)}
                empty (count (filter #(= EMPTY %) (:grid level)))]
            (when (pos? empty)
              (throw (Exception. (str (:name level) ": Left " empty " empty blocks: " (pr-str res)))))
            res))))))

(defn show-boosters [{:keys [boosters] :as level}]
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