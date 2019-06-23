(ns icfpc.level
  (:require
   [icfpc.core :refer :all]
   [icfpc.parser :as parser]
   [icfpc.writer :as writer]))

(def ^:dynamic *bot*)

(defn booster-active? [level booster]
  (-> level :bots (nth *bot*) :active-boosters (get booster 0) pos?))

(defn booster-collected? [level booster]
  (-> level :collected-boosters (get booster 0) pos?))

(defn update-bot [level key f & args]
  (apply update level :bots update *bot* update key f args))

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
              (booster-active? level DRILL)
              (not= OBSTACLE (get-level level x y))))
      level))
  ([{:keys [bots] :as level}]
    (let [{:keys [x y]} (nth bots *bot*)]
      (valid? x y level))))

(defn valid-hand? [x y dx dy {:keys [width height] :as level}]
  (let [x' (+ x dx) y' (+ y dy)]
    (when (and
            (< -1 x' width)
            (< -1 y' height)
            (every?
              (fn [[dx' dy']] (not= OBSTACLE (get-level level (+ x dx') (+ y dy'))))
              (or (hand-blocks (->Point dx dy)) (throw (Exception. (str "Unknown hand offset" dx dy))))))
      level)))

(defn bot-covering [{:keys [bots] :as level}]
  (let [{:keys [x y layout]} (nth bots *bot*)]
    (for [[dx dy] layout
          :when (if (= [0 0] [dx dy])
                  (valid? x y level)
                  (valid-hand? x y dx dy level))]
      [(+ x dx) (+ y dy)])))

(defn pick-booster [{:keys [bots boosters] :as level}]
  (let [{:keys [x y]} (nth bots *bot*)]
    (if-some [booster (boosters [x y])]
      (-> level
        (update :boosters dissoc [x y])
        (update-bot :picked-booster (constantly booster)))
      level)))

(defn wear-off-boosters [level]
  (cond-> level
    (booster-active? level FAST_WHEELS)
    (update :bots update *bot* spend :active-boosters FAST_WHEELS)

    (booster-active? level DRILL)
    (update :bots update *bot* spend :active-boosters DRILL)))

(defn drill [{:keys [bots] :as level}]
  (let [{:keys [x y]} (nth bots *bot*)]
    (if (and (booster-active? level DRILL)
             (= OBSTACLE (get-level level x y)))
      (set-level level x y WRAPPED)
      level)))

(defn mark-wrapped [{:keys [boosters] :as level}]
  (reduce
    (fn [level [x y]]
      (if (= EMPTY (get-level level x y))
        (-> level
          (set-level x y WRAPPED)
          (update :zones-area update (get-zone level x y) dec)
          (update :empty dec))
        level))
    (-> level pick-booster drill)
    (bot-covering level)))

(defn bounds [points]
  (let [xs (map first points)
        ys (map second points)]
    [(apply max xs) (apply max ys)]))

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

(defn valid-point? [{:keys [width height] :as level} [x y]]
  (and (< -1 x width) (< -1 y height)))

(defn neighbours [level [x y]]
  (filter #(valid-point? level %) [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]]))

(defn points-by-value [level value]
  (for [i (range 0 (:width level))
        j (range 0 (:height level))
        :when (= value (get-level level i j))]
    [i j]))

(defn shuffle*
  "Return a random permutation of coll"
  {:added "1.2"
   :static true}
  [^java.util.Collection coll]
  (let [al (java.util.ArrayList. coll)]
    (java.util.Collections/shuffle al (java.util.Random. 42))
    (clojure.lang.RT/vector (.toArray al))))

(defn generate-zones [level zones-count]
  (let [width (:width level)
        height (:height level)
        max-iteration-count (* width height)
        empty-points (points-by-value level EMPTY)

;        average-area 70
;        max-zones-count 1000
;        zones-count (min max-zones-count (inc (int (/ (count empty-points) average-area))))

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
        update-bot (fn [bot]
                     (assoc bot :current-zone (get-zone level (:x bot) (:y bot))))
        level (update level :bots #(mapv update-bot %))]
    level))

(defn new-bot [x y]
  {:x               x 
   :y               y
   :layout          [[0 0] [1 0] [1 1] [1 -1]]
   :active-boosters {}
   :picked-booster  nil
   :path            ""
   :current-zone    nil})

(defn load-level [name]
  (let [{:keys [bot-point corners obstacles boosters]} (parser/parse-level name)
        [width height] (bounds corners)
        init-level {:name     name
                    :width    width
                    :height   height
                    :grid     (vec (repeat (* width height) OBSTACLE))
                    :boosters (build-boosters boosters)
                    :spawns   (build-spawns boosters)
                    :collected-boosters {}
                    :bots     [(new-bot (first bot-point) (second bot-point))]}
        level (fill-level init-level corners obstacles)
        level (assoc level
                     :weights (weights level)
                     :empty   (count (filter #(= EMPTY %) (:grid level))))
        clones-count (inc (count (filter #(= CLONE (val %)) (:boosters level))))]
    (generate-zones level clones-count)))

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