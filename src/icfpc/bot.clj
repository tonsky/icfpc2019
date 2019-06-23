(ns icfpc.bot
  (:require
   [clojure.set :as set]
   [clojure.spec.alpha :as s]
   [clojure.string :as str]
   [icfpc.core :refer :all]
   [icfpc.level :refer :all])
  (:import
   [java.util HashMap HashSet ArrayDeque]))

(def ^:dynamic *disabled*)
(def ^:dynamic *explore-depth*)

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
  (when (and
          (not (*disabled* EXTRA_HAND))
          (pos? (get (:collected-boosters level) EXTRA_HAND 0)))
    (let [[x y :as p] (next-hand level)]
      (when (some? p)
        (-> level
            (update :layout conj [x y])
            (spend  :collected-boosters EXTRA_HAND)
            (update :score + 1000)
            (update :path str "B(" x "," y ")"))))))

(defn fast? [level]
  (pos? (get (:active-boosters level) FAST_WHEELS 0)))

(defn has-space? [{:keys [x y] :as level}]
  (or
    (every? #(= EMPTY (get-level level (+ x %) y OBSTACLE)) (range 1 5))
    (every? #(= EMPTY (get-level level (- x %) y OBSTACLE)) (range 1 5))
    (every? #(= EMPTY (get-level level x (+ y %) OBSTACLE)) (range 1 5))
    (every? #(= EMPTY (get-level level x (- y %) OBSTACLE)) (range 1 5))))

(defn add-fast-wheels [level]
  (when (and
          (not (*disabled* FAST_WHEELS))
          (pos? (get (:collected-boosters level) FAST_WHEELS 0))
          (not (fast? level))
          (has-space? level))
    (-> level
      (spend :collected-boosters FAST_WHEELS)
      (assoc-in [:active-boosters FAST_WHEELS] 51)
      (update :score + 1000)
      (update :path str FAST_WHEELS))))

(defn drill? [level]
  (pos? (get (:active-boosters level) DRILL 0)))

(defn add-drill [level]
  (when (and
          (not (*disabled* DRILL))
          (pos? (get (:collected-boosters level) DRILL 0))
          (not (drill? level)))
    (-> level
      (spend :collected-boosters DRILL)
      (assoc-in [:active-boosters DRILL] 31)
      (update :score + 1000)
      (update :path str DRILL))))

(defn set-beakon [level]
  (when (and
          (not (*disabled* TELEPORT))
          (pos? (get (:collected-boosters level) TELEPORT 0))
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

(defn reduplicate [{:keys [x y spawns collected-boosters] :as level}]
  (when (and
          (pos? (collected-boosters CLONE 0))
          (spawns [x y]))
    (-> level
      (update :path str REPLICATE)
      (update :bots conj {:x x :y y})
      (spend :collected-boosters CLONE))))

(defn act [level action]
  (condp = action
    UP          (move level 0 1 UP)
    DOWN        (move level 0 -1 DOWN)
    LEFT        (move level -1 0 LEFT)
    RIGHT       (move level 1 0 RIGHT)
    ; ROTATE_CW   (rotate-cw level)
    ; ROTATE_CCW  (rotate-ccw level)
    :jump0      (jump level 0)
    :jump1      (jump level 1)
    :jump2      (jump level 2)
    ; EXTRA_HAND  (add-extra-hand level)
    ; FAST_WHEELS (add-fast-wheels level) 
    ; DRILL       (add-drill level)
    ; SET_BEAKON  (set-beakon level)
    WAIT        (update level :path str WAIT)))

(def counter
  {LEFT RIGHT
   RIGHT LEFT
   UP DOWN
   DOWN UP
   ROTATE_CW ROTATE_CCW
   ROTATE_CCW ROTATE_CW})

(defn can-step? [x y drill? {:keys [width height] :as level}]
  (and (< -1 x width) (< -1 y height) (or drill? (not= OBSTACLE (get-level level x y)))))

(defn step [x y dx dy fast? drill? level]
  (let [x' (+ x dx) y' (+ y dy)]
    (when (can-step? x' y' drill? level)
      (if fast?
        (let [x'' (+ x' dx) y'' (+ y' dy)]
          (if (can-step? x'' y'' drill? level)
            (->Point x'' y'')
            (->Point x' y')))
        (->Point x' y')))))

(defn rate [[x y] {:keys [boosters weights layout width height current-zone] :as level}]
  (cond
    ;; TODO check current zone
    (boosters [x y]) 100
    ; (= EMPTY (get-level level x y)) (max 1 (aget weights (coord->idx level x y)))
    ; (= EMPTY (get-level level x y)) 1
    :else
    (reduce
      (fn [acc [dx dy]]
        (let [x' (+ x dx) y' (+ y dy)]
          (if (and
                (or
                  (= [0 0] [dx dy])
                  (valid-hand? x y dx dy level))
                (= EMPTY (get-level level x' y'))
                (= current-zone (get-zone level x y)))
            (+ acc (max 1 (aget weights (coord->idx level x' y'))))
            acc)))
      0
      layout)
    :else 0))

(defn explore [{ox :x oy :y :keys [active-boosters beakons] :as level} rate-fn]
  (let [paths (HashMap. {(->Point ox oy) []})
        queue (ArrayDeque. [[[] (->Point ox oy) (active-boosters FAST_WHEELS 0) (active-boosters DRILL 0)]])]
    (loop [max-len   *explore-depth*
           best-path nil
           best-rate 0]
      (if-some [[path [x y :as pos] fast drill :as move] (.poll queue)]
        (if (< (count path) max-len)
          ;; still exploring inside max-len
          (do
            ;; moves
            (doseq [[move dx dy] [[LEFT -1 0] [RIGHT 1 0] [UP 0 1] [DOWN 0 -1]]
                    :let [pos' (step x y dx dy (pos? fast) (pos? drill) level)]
                    :when (some? pos')
                    :when (not (.containsKey paths pos'))
                    :let [path' (conj path move)]]
              (.put paths pos' path')
              (.add queue [path' pos' (spend fast) (spend drill)]))
            ;; jumps
            (doseq [[move pos'] [[:jump0 (nth (:beakons level) 0 nil)]
                                 [:jump1 (nth (:beakons level) 1 nil)]
                                 [:jump2 (nth (:beakons level) 2 nil)]]
                    :when (some? pos')
                    :when (not (.containsKey paths pos'))
                    :let [path' (conj path move)]]
              (.put paths pos' path')
              (.add queue [path' pos' (spend fast) (spend drill)]))
            ; (prn "path" path "best-path" best-path)
            (cond+
              (empty? path)      (recur max-len best-path best-rate)
              :let [rate (/ (rate-fn pos level) (count path))]
              (zero? rate)       (recur max-len best-path best-rate)
              (zero? best-rate)  (recur max-len path rate)
              (> rate best-rate) (recur max-len path rate)
              (< rate best-rate) (recur max-len best-path best-rate)
              (< (count path) (count best-path)) (recur max-len path rate)
              :else (recur max-len best-path best-rate)))
          ;; only paths with len > max-len left, maybe already have good solution?
          (if (nil? best-path)
            (do
              (.addFirst queue move)
              (recur (+ max-len *explore-depth*) nil 0)) ;; not found anything, try expand
            best-path))
        best-path))))

(defn wait-off-fast [{:keys [active-boosters] :as level}]
  (let [fast (active-boosters FAST_WHEELS 0)]
    (when (pos? fast)
      (repeat fast WAIT))))

(defn zone-char [n]
  (if (= n 0)
    \0
    (char
     (+ (dec (int \a))
        (mod n (- (int \z) (int \a)))))))

(defn print-level [{:keys [width height name boosters x y beakons spawns bots] :as level} 
                   & {:keys [colored? max-w max-h zones?] :or {max-w 50 max-h 20 colored? true zones? false}}]
  (println name)
  (let [beakons (set beakons)]
  (doseq [y (range (min (dec height) (+ y max-h)) (dec (max 0 (- y max-h))) -1)]
    (doseq [x (range (max 0 (- x max-w)) (min width (+ x max-w)))
            :let [v (get-level level x y)
                  booster (get boosters [x y])]]
        (cond
          (or (and (= x (:x level)) (= y (:y level)))
            (some #(= [x y] [(:x %) (:y %)]) bots))
          (if colored?
            (print "\033[97;101m*\033[0m")
            (print "☺"))

          (some? booster)
          (if colored?
            (print (str "\033[97;42m" booster "\033[0m"))
            (print booster))

          (contains? spawns [x y])
          (if colored?
            (print (str "\033[97;44mX\033[0m"))
            (print "X"))

          (contains? beakons [x y])
          (if colored?
            (print (str "\033[97;44m@\033[0m"))
            (print "@"))

          (= v EMPTY)
          (if zones?
            (if colored?
              (print "\033[103m" (zone-char (get-zone level x y)) "\033[0m")
              (print (get-zone level x y)))
            (if colored?
              (print (str "\033[103m" (zone-char (get-zone level x y)) "\033[0m"))
              (print "•")))

          (= v WRAPPED)
          (if colored?
            (print "\033[97;43m.\033[0m")
            (print "+"))

          (= v OBSTACLE)
          (print ".")

          :else
          (print (get-level level x y))))
    (println)))
  (println))

(defn print-step [level delay]
  (println "\033[2J")
  (print-level level)
  (println "Active:" (filter #(pos? (second %)) (:active-boosters level)) "collected:" (:collected-boosters level))
  (println "Hands:" (dec (count (:layout level))) "layout:" (:layout level))
  (println "Beakons:" (:beakons level))
  (println "Score:" (path-score (:path level)) #_#_"via" (:path level))
  (println "Zone: " (zone-char (:current-zone level)))
  ; (println "Areas:" (:zones-area level))
  (when (some? delay)
    (Thread/sleep delay)))

(defn collect-clone [{:keys [boosters collected-boosters] :as level}]
  (when (and
          (= 0 (collected-boosters CLONE 0))
          (some (fn [[_ b]] (= b CLONE)) boosters))
    (explore level (fn [[x y] level]
                     (if (= (boosters [x y]) CLONE) 1 0)))))

(defn goto-spawn [{:keys [x y spawns collected-boosters] :as level}]
  (when (pos? (collected-boosters CLONE 0))
    (when-not (spawns [x y])
      (explore level (fn [[x y] level]
                       (if (spawns [x y]) 1 0))))))

(defn solve [level & [{:keys [debug? delay disabled explore-depth]
                       :or {debug? true, disabled #{}, explore-depth 10}}]]
  (let [t0 (System/currentTimeMillis)
        *last-frame (atom 0)]
    (binding [*disabled*      disabled
              *explore-depth* explore-depth]
      (loop [level (mark-wrapped level)]
        (when (.isInterrupted (Thread/currentThread))
          (throw (InterruptedException.)))

        (when (or (some? delay)
                (and debug? (>= (- (System/currentTimeMillis) @*last-frame) 200)))
          (print-step level delay)
          (reset! *last-frame (System/currentTimeMillis)))
        (cond+
          (= 0 (:empty level))
          (do
            (when debug? (print-step level delay))
            {:path  (:path level)
             :score (path-score (:path level))
             :time  (- (System/currentTimeMillis) t0)})

          (= 0 (zone-area level (:current-zone level)))
          (let [next-zone (closest-zone level (:x level) (:y level))]
            (recur (assoc level :current-zone next-zone)))

          :when-some [level' (or
                               (reduplicate level)
                               (add-extra-hand level)
                               (add-fast-wheels level) 
                               (add-drill level)
                               (set-beakon level))]
          (recur (wear-off-boosters level'))

          :when-some [path (or 
                             (goto-spawn level)
                             (collect-clone level)
                             (explore level rate)
                             (wait-off-fast level))]
          (recur
            (reduce
              (fn [acc action]
                (when-not (identical? acc level)
                  (when (some? delay)
                    (print-step acc delay)))
                (if-some [acc' (some-> acc (act action) (wear-off-boosters))]
                  (if (> (:score acc') (:score level))
                    (reduced acc')
                    acc')
                  (throw (Exception. (str "Wow path failed from" (:x level) (:y level) "path" path)))))
              level
              path))

          :else
          (do
            (print-step level nil)
            (throw (Exception. (str "Stuck at " (:name level) ", left " (:empty level) " empty blocks, score: " (:score level) ", path: " (:path level))))))))))

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