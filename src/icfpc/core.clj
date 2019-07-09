(ns icfpc.core)

(defn queue [& xs]
  (into clojure.lang.PersistentQueue/EMPTY xs))

(defn spy [x]
  (println x)
  x)

(defmacro cond+ [& clauses]
  (when clauses
    (let [[c1 c2 & cs] clauses]
      (cond
        (< (count clauses) 2) (throw (IllegalArgumentException. "cond requires an even number of forms"))
        (= c1 :let)          `(let ~c2 (cond+ ~@cs))
        (= c1 :do)           `(do ~c2 (cond+ ~@cs))
        (= c1 :when-some)    `(if-some ~c2 ~(first cs) (cond+ ~@(next cs)))
        :else                `(if ~c1 ~c2 (cond+ ~@cs))))))

(defrecord Point [x y]
  Object
  (toString [_] (str "(" x "," y ")"))
  clojure.lang.Indexed
  (nth [_ i] (case i 0 x 1 y))
  (nth [_ i nf] (case i 0 x 1 y nf)))

(def EMPTY (byte 0))
(def OBSTACLE (byte 1))
(def WRAPPED (byte 2))
(def EXTRA_HAND \B)
(def FAST_WHEELS \F)
(def DRILL \L)
(def SPAWN \X)
(def TELEPORT \R)
(def CLONE \C)
(def WAIT \Z)
(def UNKNOWN \?)

(def UP    \W)
(def DOWN  \S)
(def LEFT  \A)
(def RIGHT \D)
(def ROTATE_CW  \E)
(def ROTATE_CCW \Q)
(def SET_BEAKON \R)
(def JUMP       \T)
(def REPLICATE  \C)

(defn spend
  ([v] (cond (nil? v) nil (> v 1) (dec v) :else 0))
  ([map k k2]
   (let [v ((map k) k2)]
     (cond
       (nil? v) map
       (> v 1)  (update map k assoc k2 (dec v))
       :else    (update map k dissoc k2)))))

(defn coord->idx [level x y] (+ x (* y (:width level))))

(defn get-level
  ([level x y]
   (aget (:grid level) (coord->idx level x y)))
  ([level x y default]
   (if (and
         (< -1 x (:width level))
         (< -1 y (:height level)))
     (aget (:grid level) (coord->idx level x y))
     default)))

(defn set-level [level x y value]
  (aset-byte (:grid level) (coord->idx level x y) value)
  level)

(defn get-zone [level x y]
  (aget (:zones-grid level) (coord->idx level x y)))

(defn zone-area [level zone]
  (get (:zones-area level) zone))

(defn seek [pred coll]
  (some #(if (pred %) %) coll))

(defn path-score [path]
  (count (re-seq #"[A-Z]" path)))

(defn sol-score [sol]
  (->> (clojure.string/split sol #"#")
    (map path-score)
    (reduce max)))

(defn level-score [level]
  (->> (:bots level)
    (map #(path-score (:path %)))
    (reduce max)))

(defn arr-reduce [f init arr]
  (areduce arr i ret init
    (f ret (aget arr i))))
