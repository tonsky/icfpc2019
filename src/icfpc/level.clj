(ns icfpc.level
  (:require
   [icfpc.core :refer :all]
   [icfpc.parser :as parser]
   [icfpc.writer :as writer]))

(defn valid-point? [{:keys [width height] :as level} [x y]]
  (and (< -1 x width) (< -1 y height)))

(defn is-booster-active [level booster]
  (pos? (get (:active-boosters level) booster 0)))

(defn round-down [x]
  (if (= x (int x))
    (dec (int x))
    (int x)))

(defn round-up [x]
  (int x)
  #_(if (= x (int x))
    (int x)
    (int x)))

(defn ray-path-general [from-point to-point]
  (let [[from-x from-y :as from] (min-key first from-point to-point)
        [to-x to-y :as to] (max-key first from-point to-point)]
    (if (= from-x to-x)
      (mapv (fn [y] [from-x y]) (range from-y (inc to-y)))
      (let [k (/ (- to-y from-y) (- to-x from-x))]
        (concat
         (let [[low high] (if (< from-y (+ from-y 1/2 (* k 1/2)))
                            [from-y (round-down (+ from-y 1/2 (* k 1/2)))]
                            [(round-up (+ from-y 1/2 (* k 1/2))) from-y])]
           (map (fn [y] [from-x y]) (range low (inc high))))
         (mapcat
          (fn [dx]
            (let [[low high] (sort [(+ from-y 1/2 (* k (- dx 1/2)))
                                    (+ from-y 1/2 (* k (+ dx 1/2)))])
                  low' (round-up low)
                  high' (round-down high)]
              (map (fn [y] [(+ from-x dx) y]) (range low' (inc high')))))
          (range 1 (- to-x from-x)))
         (let [[low high] (if (<= (- (+ to-y 1/2) (* k 1/2)) to-y)
                            [(round-up (- (+ to-y 1/2) (* k 1/2))) to-y]
                            [to-y (round-down (- (+ to-y 1/2) (* k 1/2)))])]
           (map (fn [y] [to-x y]) (range low (inc high)))))))))

(def hand-blocks
  (into {(->Point 1 -1) [[1 -1]]
         (->Point 1 0)  [[1 0]]
         (->Point 1 1)  [[1 1]]}
    (for [maxy (range 2 20)]
      [(->Point 1 maxy) (vec
                          (concat
                            (for [y (range 1 (inc (quot maxy 2)))] [0 y])
                            (for [y (range (int (Math/ceil (/ maxy 2.0))) (inc maxy))] [1 y])))])))

(defn valid?
  ([x y {:keys [width height] :as level}]
    (when (and
            (< -1 x width)
            (< -1 y height)
            (or
              (is-booster-active level DRILL)
              (not= OBSTACLE (get-level level x y))))
      level))
  ([{:keys [x y] :as level}]
    (valid? x y level)))

(defn valid-hand? [x y dx dy {:keys [width height] :as level}]
  (let [x' (+ x dx) y' (+ y dy)]
    (when (and
            (< -1 x' width)
            (< -1 y' height)
            (every?
              (fn [[dx' dy']] (not= OBSTACLE (get-level level (+ x dx') (+ y dy'))))
              (or (hand-blocks (->Point dx dy)) (throw (Exception. (str "Unknown hand offset" dx dy))))))
      level)))

(defn bot-covering [{:keys [x y layout] :as level}]
  (for [[dx dy] layout
        :when (if (= [0 0] [dx dy])
                (valid? x y level)
                (valid-hand? x y dx dy level))]
    [(+ x dx) (+ y dy)]))

(defn collect-booster [{:keys [boosters] :as level}]
  (if-some [booster (get boosters [(:x level) (:y level)])]
    (-> level
      (update :boosters dissoc [(:x level) (:y level)])
      (update :collected-boosters update booster (fnil inc 0))
      (update :score + 100))
    level))

(defn wear-off-boosters [level]
  (-> level
    (spend :active-boosters FAST_WHEELS)
    (spend :active-boosters DRILL)))

(defn score-point [level x y]
  (get {EMPTY    1
        OBSTACLE 0
        WRAPPED  0}
    (get-level level x y)))

(defn score-point' [level x y]
  (if (= EMPTY (get-level level x y))
    (max 1 (aget (:weights level) (coord->idx level x y)))
    0))

(defn drill [{:keys [x y] :as level}]
  (if (and (is-booster-active level DRILL)
           (= OBSTACLE (get-level level x y)))
    (set-level level x y WRAPPED)
    level))

(defn mark-wrapped
  "Apply wrapped to what bot at current pos touches"
  [{:keys [boosters] :as level}]
  (reduce
    (fn [level [x y]]
      (let [before  (get-level level x y)
            booster (get boosters [x y])]
        (cond-> level
          (= EMPTY before) (-> (set-level x y WRAPPED)
                               (update-in [:zones-area (get-zone level x y)] dec)
                               (update :empty dec))
          true             (update :score + (score-point' level x y)))))
    (-> level collect-booster drill)
    (bot-covering level)))

(def prob-001
  {:width  8
   :height 3
   :grid [EMPTY EMPTY EMPTY EMPTY EMPTY EMPTY OBSTACLE OBSTACLE
          EMPTY EMPTY EMPTY EMPTY EMPTY EMPTY EMPTY    EMPTY
          EMPTY EMPTY EMPTY EMPTY EMPTY EMPTY OBSTACLE OBSTACLE]
   :x 0
   :y 0
   :layout [[0 0] [1 0] [1 1] [1 -1]]
   :boosters {}})

(defn bounds [points]
  (let [xs (map first points)
        ys (map second points)]
    [(apply max xs) (apply max ys)]))

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

#_(defn fill-poly [level corners value]
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
                  (fill-dfs level [mid-x mid-y] value)

                  [:up :right]
                  (fill-dfs level [(dec mid-x) mid-y] value)

                  [:left :up]
                  (fill-dfs level [(dec mid-x) (dec mid-y)] value)

                  [:down :left]
                  (fill-dfs level [mid-x (dec mid-y)] value)

                  level)))
            level
            (partition 3 1 (into corners (take 2 corners))))))

(defn vertical-segments [corners]
  (let [segments (partition 2 1 (into corners (take 1 corners)))]
    (keep (fn [[[from-x from-y] [to-x to-y]]]
            (when (= from-x to-x)
              {:x from-x
               :from-y (min from-y to-y)
               :to-y (max from-y to-y)}))
          segments)))

(defn fill-level [level corners obstacles]
  (let [segments (sort-by :x (apply concat (into [(vertical-segments corners)] (map vertical-segments obstacles))))]
    (reduce (fn [level y]
              (let [xs (map :x (filter (fn [{:keys [from-y to-y]}]
                                         (and (<= from-y y) (< y to-y)))
                                       segments))
                    rs (take-nth 2 (partition 2 1 xs))]
                (reduce (fn [level [from-x to-x]]
                          (reduce (fn [level x]
                                    (set-level level x y EMPTY))
                                  level
                                  (range from-x to-x)))
                        level
                        rs)))
            level
            (range (:height level)))))

(defn build-boosters [boosters]
  (into {}
    (for [[b [x y]] boosters
          :when (not= b SPAWN)]
      [[x y] b])))

(defn build-spawns [boosters]
  (into #{}
    (for [[b [x y]] boosters
          :when (= b SPAWN)]
      [x y])))

(defn weights [{:keys [width height] :as level}]
  (short-array
    (for [y (range 0 height)
          x (range 0 width)]
      (reduce + 0
        (for [[dx dy] [[0 1] [0 -1] [-1 0] [1 0] [1 1] [-1 -1] [-1 1] [1 -1]]
              :let [x' (+ x dx)
                    y' (+ y dy)]]
          (if (or (< x' 0) (>= x' width) (< y' 0) (>= y' height)
                (= (get-level level x' y') OBSTACLE))
            1
            0))))))

(defn neighbours [level [x y]]
  (filter #(valid-point? level %) [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]]))

(defn build-path [parents point]
  (loop [path []
         current point
         depth 0]
    (if-let [parent (get parents current)]
      (if (or (< 10000 depth) (= parent current))
        (do
          (prn "PATH TOO LONG!")
          (conj path current))
        (recur (conj path current) parent (inc depth)))
      path)))

(defn bfs [level [x y :as start] target block]
  (loop [q (queue start)
         parents {start nil}]
    (if-let [p (peek q)]
      (let [ns (filter (fn [[x y :as n]]
                         (and
                          (not (contains? parents n))
                          (not= (get-level level x y) block)))
                       (neighbours level p))
            parents' (reduce (fn [ps n]
                               (assoc ps n p))
                             parents ns)
            finded (filter (fn [[x y :as n]]
                             (= (get-level level x y) target)) ns)]
        (if (not-empty finded)
          (build-path parents' (first finded))
          (recur (into (pop q) ns) parents')))
      (prn "CANT FIND PATH!"))))

(defn fill-connected-component [level points target block]
  (let [[i-x i-y]  (first points)
        include-last (rest points)
        level (reduce (fn [level next-include]
                        (let [path (bfs level next-include target block)]
                          (reduce (fn [level [px py]]
                                    (set-level level px py target))
                                  level
                                  path)))
                      (set-level level i-x i-y target)
                      include-last)]
    (reduce (fn [level [x y]]
              (set-level level x y target))
            level
            points)))

(defn fill-bfs [level [x y :as start] target block]
  (loop [q (queue start)
         parents {start nil}
         level level]
    (if-let [p (peek q)]
      (let [ns (filter (fn [[x y :as n]]
                         (and
                          (not (contains? parents n))
                          (not= (get-level level x y) block)))
                       (neighbours level p))
            parents' (reduce (fn [ps n]
                               (assoc ps n p))
                             parents ns)]
        (recur (into (pop q) ns) parents' (reduce (fn [level [x y]]
                                                    (set-level level x y target))
                                                  level
                                                  ns)))
      level)))

(defn points-by-value [level value]
  (for [i (range 0 (:width level))
        j (range 0 (:height level))
        :when (= value (get-level level i j))]
    [i j]))

(defn min-area [t-size]
  (int (Math/ceil (* 0.2 (* t-size t-size)))))

(defn inflate [level]
  (let [empty-fields (points-by-value level EMPTY)
        borders (filter
                 (fn [[x y]] (not= (get-level level x y) OBSTACLE))
                 (clojure.set/difference (into #{} (mapcat #(neighbours level %) empty-fields))
                                         (into #{} empty-fields)))]
    (reduce
     (fn [level [x y]]
       (set-level level x y EMPTY))
     level
     borders)))

(defn inflate-min-area [level min-area]
  (if (< (count (points-by-value level EMPTY)) min-area)
    (recur (inflate level) min-area)
    level))

(defn place-boosters [level boosters]
  (let [availible-positions (shuffle (points-by-value level EMPTY))]
    (reduce
     (fn [level [i b]]
       (let [[x y] (get availible-positions i)]
             (update level :boosters assoc [x y] b)))
     level
     (map (fn [i v] [i v])
          (range)
          (mapcat (fn [[booster count]]
                    (repeat count
                     (case booster
                       :extra-hands EXTRA_HAND
                       :fast-wheels FAST_WHEELS
                       :drills DRILL
                       :teleports TELEPORT
                       :cloning CLONE
                       :spawns SPAWN)))
                  boosters)))))

(defn place-bot [level]
  (let [[x y] (first (shuffle (points-by-value level EMPTY)))]
    (assoc level :x x :y y )))

(defn add-edge-bottom [level segments puzzle]
  (let [exclude (set (:exclude puzzle))]
    (first
      (for [[[x y] [x' y']] (map vector segments (next (cycle segments)))
            :when (and (= y y') (> y 0) (< x x') (>= (- x' x) 3))
            :let  [y'' (dec y)]
            x''   (range (inc x) (dec x'))
            :when (not (exclude [x'' y'']))]
        [x'' y'']))))

(defn add-edge-top [level segments puzzle]
  (let [exclude (set (:exclude puzzle))]
    (first
      (for [[[x y] [x' y']] (map vector segments (next (cycle segments)))
            :when (and (= y y') (< y (:height level)) (< x' x) (>= (- x x') 3))
            :let  [y'' y]
            x''   (range (inc x') (dec x))
            :when (not (exclude [x'' y'']))]
        [x'' y'']))))

(defn add-edge-left [level segments puzzle]
  (let [exclude (set (:exclude puzzle))]
    (first
      (for [[[x y] [x' y']] (map vector segments (next (cycle segments)))
            :when (and (= x x') (> x 0) (< y' y) (>= (- y y') 3))
            :let  [x'' (dec x)]
            y''   (range (inc y') (dec y))
            :when (not (exclude [x'' y'']))]
        [x'' y'']))))

(defn add-edge-right [level segments puzzle]
  (let [exclude (set (:exclude puzzle))]
    (first
      (for [[[x y] [x' y']] (map vector segments (next (cycle segments)))
            :when (and (= x x') (< x (:width level)) (< y y') (>= (- y' y) 3))
            :let  [x'' x]
            y''   (range (inc y) (dec y'))
            :when (not (exclude [x'' y'']))]
        [x'' y'']))))

(defn add-edges [level puzzle]
  (let [segments (writer/segments level)]
    (if (>= (count segments) (:v-min puzzle))
      level
      (let [[x y] (or (add-edge-bottom level segments puzzle)
                    (add-edge-top   level segments puzzle)
                    (add-edge-left  level segments puzzle)
                    (add-edge-right level segments puzzle)
                    (throw (Exception. (str "Can’t add enough edges: has " (count segments) " need " (:v-min puzzle)))))]
        (recur (set-level level x y EMPTY) puzzle)))))

(defn add-boundaries [{:keys [width height] :as level} puzzle]
  (let [exclude (set (:exclude puzzle))
        p1      (->> (map (fn [y] [2 y]) (range 2 (- height 2)))
                  (seek #(not (exclude %))))
        p2      (->> (map (fn [y] [(- width 2) y]) (range 2 (- height 2)))
                  (reverse)
                  (seek #(not (exclude %))))]
    (update puzzle :include into [p1 p2])))

(defn generate-level [puzzle-name]
  (let [puzzle (parser/parse-puzzle puzzle-name)
        t-size (:t-size puzzle)
        init-level {:name               (str puzzle-name ".desc")
                    :width              t-size
                    :height             t-size
                    :grid               (vec (repeat (* t-size t-size) UNKNOWN))
                    :boosters           {}
                    :x                  0
                    :y                  0
                    :layout             [[0 0] [1 0] [1 1] [1 -1]]
                    :collected-boosters {}
                    :active-boosters    {}
                    :score              0
                    :path               ""}
        level (reduce (fn [level [x y]]
                        (set-level level x y OBSTACLE))
                      init-level
                      (:exclude puzzle))
        puzzle (add-boundaries level puzzle)
        level (fill-connected-component level (:include puzzle) EMPTY OBSTACLE)
        level (reduce
               (fn [level [x y]]
                 (set-level level x y UNKNOWN))
               level
               (:exclude puzzle))
        level (fill-connected-component level (:exclude puzzle) OBSTACLE EMPTY)
        level (inflate-min-area level (min-area (:t-size puzzle)))
        level (fill-bfs level (last (:exclude puzzle)) OBSTACLE EMPTY)
        level (reduce (fn [level [x y]]
                        (set-level level x y EMPTY))
                      level
                      (points-by-value level UNKNOWN))
        level (add-edges level puzzle)
        level (place-boosters level (select-keys puzzle [:extra-hands :fast-wheels :drills :teleports :cloning :spawns]))
        level (place-bot level)]
    level))

(defn shuffle*
  "Return a random permutation of coll"
  {:added "1.2"
   :static true}
  [^java.util.Collection coll]
  (let [al (java.util.ArrayList. coll)]
    (java.util.Collections/shuffle al (java.util.Random. 42))
    (clojure.lang.RT/vector (.toArray al))))

(defn generate-zones [level]
  (let [average-area 100
        width (:width level)
        height (:height level)
        max-iteration-count (* width height)
        empty-points (points-by-value level EMPTY)
        zones-count (inc (int (/ (count empty-points) average-area)))
        centers (map-indexed (fn [idx z] [(inc idx) z]) (take zones-count (shuffle* empty-points)))
        zones-map {:width width
                   :height height
                   :grid (vec (repeat (* width height) 0))}
        zones-map (reduce (fn [zm [idx [x y]]]
                            (set-level zm x y idx))
                          zones-map
                          centers)
        zones-map (loop [zones zones-map
                         iteration 0]
                    (let [{:keys [zm end?]} (reduce (fn [{:keys [zm end?]} [x y]]
                                                      (if (= (get-level zm x y) 0)
                                                        (let [z (first (keep (fn [[nx ny]]
                                                                               (when (= EMPTY (get-level level nx ny))
                                                                                 (let [z (get-level zones nx ny)]
                                                                                   (when (not= z 0)
                                                                                     z))))
                                                                             (neighbours zm [x y])))]
                                                          (if (some? z)
                                                            {:zm (set-level zm x y z)
                                                             :end? end?}
                                                            {:zm zm
                                                             :end? false}))
                                                        {:zm zm :end? end?}))
                                                    {:zm zones :end? true}
                                                    empty-points)]
                      (if (and (< iteration max-iteration-count) (not end?))
                        (recur zm (inc iteration))
                        (do
                          (when (= iteration max-iteration-count)
                            (println "Can’t generate zones for" (:name level)))
                          zm))))
        zones-area (into {}
                         (map
                          (fn [[z points]]
                            [z (count points)])
                          (group-by first (for [x (range width)
                                                y (range height)
                                                :when (= EMPTY (get-level level x y))]
                                            [(get-level zones-map x y) [x y]]))))
        level (assoc level :zones-grid (:grid zones-map)
                           :zones-area zones-area)
        level (assoc level :current-zone (get-zone level (:x level) (:y level)))]
    level))

(defn load-level [name]
  (let [{:keys [bot-point corners obstacles boosters]} (parser/parse-level name)
        [width height] (bounds corners)
        init-level {:name               name
                    :width              width
                    :height             height
                    :grid               (vec (repeat (* width height) OBSTACLE))
                    :boosters           (build-boosters boosters)
                    :spawns             (build-spawns boosters)
                    :x                  (first bot-point)
                    :y                  (second bot-point)
                    :layout             [[0 0] [1 0] [1 1] [1 -1]]
                    :collected-boosters {}
                    :active-boosters    {}
                    :score              0
                    :path               ""}
        level (fill-level init-level corners obstacles)
        level (assoc level
                     :weights (weights level)
                     :empty   (count (filter #(= EMPTY %) (:grid level))))]
    (generate-zones level)))

;; closest not filled zone
(defn closest-zone [level x y]
  (loop [q (queue [x y])
         visited #{[x y]}]
    (if-let [p (peek q)]
      (let [ns (filter (fn [[x y :as n]]
                         (and
                          (not (contains? visited n))
                          (not= (get-level level x y) OBSTACLE)))
                       (neighbours level p))
            finded (first (keep (fn [[x y :as n]]
                                  (let [zone-id (get-zone level x y)
                                        area (zone-area level zone-id)]
                                    (when (and (some? area) (pos? area))
                                      zone-id)))
                                ns))]
        (if (some? finded)
          finded
          (recur (into (pop q) ns) (into visited ns))))
      nil)))

(comment
  (def lvl (load-level "prob-002.desc"))
  (apply max (vals (:zones-area lvl)))
  (apply min (vals (:zones-area lvl)))
  (get-level lvl 5 5)
  (map (get-level lvl (first %) (second %))
       (neighbours lvl [5 5]))
  (get-level lvl 11 0)
  (:zones-grid lvl)

  (closest-zone (assoc-in lvl [:zones-area 1] 0) 16 0)

  (count (points-by-value lvl' EMPTY))

#{:block-number
  :epoch
  :t-size
  :v-min
  :v-max
  :extra-hands
  :fast-wheels
  :drills
  :teleports
  :cloning
  :spawns
  :include
  :exclude}


  (def puzzle (parser/parse-puzzle "puzzle.cond"))
  (def lvl (generate-level "puzzle.cond"))
  (+ 1 (count (icfpc.writer/segments lvl)))
  (spit "puzzle-solved.desc" (icfpc.writer/desc lvl))

  (:boosters lvl)

  (select-keys puzzle [:extra-hands
  :fast-wheels
  :drills
  :teleports
  :cloning
  :spawns])

  *e

  (def puzzle (parser/parse-puzzle "first.cond"))
  (def lvl (generate-level "first.cond"))
  (spit "first.desc" (icfpc.writer/desc lvl))


  (count (icfpc.writer/segments lvl))



  (:width lvl)
  (:height lvl)

  (icfpc.bot/print-level lvl :colored? false :max-w 1000 :max-h 1000)
  (icfpc.parser/validate-puzzle (parser/parse-puzzle "first.cond") lvl)

  (keys puzzle)
  (:t-size puzzle)
  (:v-min puzzle)
  (:v-max puzzle)
  (count (:include puzzle))
  (count (:exclude puzzle))


  (= (ray-path [1 1] [3 0]) [[1 1] [2 0] [2 1] [3 0]])
  (= (ray-path [1 1] [3 2]) [[1 1] [2 1] [2 2] [3 2]])

  (= (ray-path [0 1] [3 0]) [[0 1] [1 1] [2 0] [3 0]])

  (= (ray-path [0 0] [3 1]) [[0 0] [1 0] [2 1] [3 1]])


  (= (ray-path [0 0] [3 3]) [[0 0] [1 1] [2 2] [3 3]])
  (= (ray-path [5 5] [1 1]) [[1 1] [2 2] [3 3] [4 4] [5 5]])
  (= (ray-path [0 5] [5 0]) [[0 5] [1 4] [2 3] [3 2] [4 1] [5 0]])

  (def lvls (mapv (fn [n]
                    (load-level (format "prob-%03d.desc" n)))
                  (range 1 51)))

  (build-boosters
   (:boosters (parser/parse-level "prob-050.desc")))

  (doseq [lvl lvls]
    (icfpc.bot/print-level lvl))


  (into [(vertical-segments corners)] (map vertical-segments obstacles))


  (icfpc.bot/print-level (load-level "prob-150.desc"))

  (def test-level
    {:width  10
     :height 8
     :x 1
     :y 1
     :grid   [OBSTACLE OBSTACLE OBSTACLE OBSTACLE OBSTACLE OBSTACLE OBSTACLE OBSTACLE OBSTACLE OBSTACLE 
              OBSTACLE EMPTY    EMPTY    EMPTY    OBSTACLE EMPTY    EMPTY    EMPTY    EMPTY    OBSTACLE 
              OBSTACLE EMPTY    EMPTY    EMPTY    EMPTY    EMPTY    EMPTY    EMPTY    EMPTY    OBSTACLE
              OBSTACLE EMPTY    EMPTY    EMPTY    EMPTY    EMPTY    EMPTY    EMPTY    EMPTY    OBSTACLE
              OBSTACLE EMPTY    EMPTY    EMPTY    EMPTY    EMPTY    EMPTY    EMPTY    EMPTY    OBSTACLE
              OBSTACLE EMPTY    EMPTY    EMPTY    EMPTY    EMPTY    EMPTY    EMPTY    EMPTY    OBSTACLE
              OBSTACLE EMPTY    EMPTY    EMPTY    EMPTY    EMPTY    EMPTY    EMPTY    EMPTY    OBSTACLE
              OBSTACLE OBSTACLE OBSTACLE OBSTACLE OBSTACLE OBSTACLE OBSTACLE OBSTACLE OBSTACLE OBSTACLE]})

  (def test-puzzle
    {:v-min 44
     :exclude [[5 0] [2 3] [3 3]]})

  (icfpc.bot/print-level (icfpc.level/add-edges test-level test-puzzle))
  )