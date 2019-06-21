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