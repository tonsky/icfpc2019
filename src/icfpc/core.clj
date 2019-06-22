(ns icfpc.core)

(defn queue [& xs]
  (into clojure.lang.PersistentQueue/EMPTY xs))

(def EMPTY \•)
(def OBSTACLE \X)
(def WRAPPED \+)
(def EXTRA_HAND \B)
(def FAST_WHEELS \F)
(def DRILL \L)
(def X_UNKNOWN_PERK \?)

(def UP    \W)
(def DOWN  \S)
(def LEFT  \A)
(def RIGHT \D)
(def ROTATE_CW \E)
(def ROTATE_CCW \Q)

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