(ns icfpc.core)

(defn queue [& xs]
  (into clojure.lang.PersistentQueue/EMPTY xs))

(def EMPTY \•)
(def OBSTACLE \O)
(def WRAPPED \+)
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

(defn max-by [compare-fn xs]
  (loop [max-key   (first xs)
         max-value (compare-fn max-key)
         xs        (next xs)]
    (cond
      (empty? xs)
      max-key

      (pos? (compare (compare-fn (first xs)) max-value))
      (recur (first xs) (compare-fn (first xs)) (next xs))

      :else
      (recur max-key max-value (next xs)))))

(defn spend [map key]
  (let [v (get map key)]
    (cond
      (nil? v) map
      (> v 1)  (assoc map key (dec v))
      :else    (dissoc map key))))

(defn coord->idx [level x y] (+ x (* y (:width level))))

(defn get-level [level x y]
  (nth (:grid level) (coord->idx level x y)))

(defn set-level [level x y value]
  (update level :grid assoc (coord->idx level x y) value))