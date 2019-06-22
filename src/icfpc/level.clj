(ns icfpc.level
  (:require [icfpc.core :refer :all]
            [icfpc.parser :as parser]))

(defn coord->idx [level x y] (+ x (* y (:width level))))

(defn get-level [level x y]
  (nth (:grid level) (coord->idx level x y)))

(defn valid-point? [{:keys [width height] :as level} [x y]]
  (and (< -1 x width) (< -1 y height)))

(defn set-level [level x y value]
  (update level :grid assoc (coord->idx level x y) value))

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

(defn ray-path [from-point to-point]
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

(defn visible? [level from to]
  (every? true? (map (fn [[x y :as p]]
                       (not= OBSTACLE (get-level level x y)))
                     (ray-path from to))))

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

(defn bot-covering [{:keys [x y layout] :as level}]
  (reduce 
    (fn [acc [dx dy]]
      (let [x' (+ x dx)
            y' (+ y dy)]
        (if (valid? x' y' level)
          (conj acc [x' y'])
          acc)))
    []
    layout))

(defn collect-booster [{:keys [boosters] :as level}]
  (let [booster (get boosters [(:x level) (:y level)])]
    (if (some? booster)
      (-> level
          (update :boosters (fn [boosters]
                              (dissoc boosters [(:x level) (:y level)])))
          (update :collected-boosters (fn [collected-boosters]
                                        (if (contains? collected-boosters booster)
                                          (update collected-boosters booster inc)
                                          (assoc collected-boosters booster 1))))
          (update :score + 100))
      level)))

(defn wear-off-boosters [level]
  (let [fast-wheels (get-in level [:active-boosters FAST_WHEELS] 0)
        drill       (get-in level [:active-boosters DRILL] 0)]
    (cond-> level
      (pos? fast-wheels) (update-in [:active-boosters FAST_WHEELS] dec)
      (pos? drill) (update-in [:active-boosters DRILL] dec))))

(defn score-point [level x y]
  (get {EMPTY    1
        OBSTACLE 0
        WRAPPED  0}
    (get-level level x y)))

(defn score-point' [level x y]
  (if (= EMPTY (get-level level x y))
    (max 1 (nth (:weights level) (coord->idx level x y)))
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
          (= EMPTY before) (set-level x y WRAPPED)
          true             (update :score + (score-point' level x y)))))
    (-> level collect-booster drill)
    (bot-covering level)))

(def prob-001
  {:width  7
   :height 3
   :grid [EMPTY EMPTY EMPTY EMPTY EMPTY OBSTACLE OBSTACLE
                EMPTY EMPTY EMPTY EMPTY EMPTY EMPTY EMPTY
                EMPTY EMPTY EMPTY EMPTY EMPTY OBSTACLE OBSTACLE]
   :x 0
   :y 0
   :layout [[0 0] [1 0] [1 1] [1 -1]]
   :boosts {EXTRA_HAND 0
            FAST_WHEELS 0
            DRILL 0
            X_UNKNOWN_PERK 0}})

(defn bounds [points]
  (let [xs (map first points)
        ys (map second points)]
    [(inc (apply max xs)) (inc (apply max ys))]))

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

(defn fill-dfs [level [x y :as current] value]
  (if (and (valid-point? level current) (not= (get-level level x y) value))
    (let [level (set-level level x y value)]
      (-> level
          (fill-dfs [(inc x) y] value)
          (fill-dfs [x (inc y)] value)
          (fill-dfs [(dec x) y] value)
          (fill-dfs [x (dec y)] value)))
    level))



(defn fill-poly [level corners value]
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

(defn collect-boosters [boosters]
    (into {}
          (map (fn [[b [x y]]]
                 [[x y] b]))
          boosters))

(defn weights [{:keys [width height] :as level}]
  (for [y (range 0 height)
        x (range 0 width)]
    (reduce + 0
      (for [[dx dy] [[0 1] [0 -1] [-1 0] [1 0] [1 1] [-1 -1] [-1 1] [1 -1]]
            :let [x' (+ x dx)
                  y' (+ y dy)]]
        (if (or (< x' 0) (>= x' width) (< y' 0) (>= y' height)
              (= (get-level level x' y') OBSTACLE))
          1
          0)))))

(defn load-level [name]
  (let [{:keys [bot-point corners obstacles boosters]} (parser/parse-level name)
        [width height] (bounds corners)
        init-level {:name               name
                    :width              width
                    :height             height
                    :grid               (vec (repeat (* width height) OBSTACLE))
                    :boosters           (collect-boosters boosters)
                    :x                  (first bot-point)
                    :y                  (second bot-point)
                    :layout             [[0 0] [1 0] [1 1] [1 -1]]
                    :collected-boosters {}
                    :active-boosters    {}
                    :score              0
                    :path               ""}
        level (fill-level init-level corners obstacles)]
    (assoc level :weights (weights level))))

(comment
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

  (collect-boosters
   (:boosters (parser/parse-level "prob-050.desc")))

  (doseq [lvl lvls]
    (icfpc.bot/print-level lvl))


  (into [(vertical-segments corners)] (map vertical-segments obstacles))


  (icfpc.bot/print-level (load-level "prob-150.desc"))


  )