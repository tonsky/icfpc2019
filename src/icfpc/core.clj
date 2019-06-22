(ns icfpc.core)

(defn queue [& xs]
  (into clojure.lang.PersistentQueue/EMPTY xs))

(defmacro cond+ [& clauses]
  (when clauses
    (let [[c1 c2 & cs] clauses]
      (cond
        (< (count clauses) 2) (throw (IllegalArgumentException. "cond requires an even number of forms"))
        (= c1 :let)          `(let ~c2 (cond+ ~@cs))
        (= c1 :do)           `(do ~c2 (cond+ ~@cs))
        (= c1 :when-some)    `(if-some ~c2 ~(first cs) (cond+ ~@(next cs)))
        :else                `(if ~c1 ~c2 (cond+ ~@cs))))))

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

(defn spend [map k k2]
  (let [v ((map k) k2)]
    (cond
      (nil? v) map
      (> v 1)  (update map k assoc k2 (dec v))
      :else    (update map k dissoc k2))))

(defn coord->idx [level x y] (+ x (* y (:width level))))

(defn get-level [level x y]
  (nth (:grid level) (coord->idx level x y)))

(defn set-level [level x y value]
  (update level :grid assoc (coord->idx level x y) value))

(defn seek [pred coll]
  (some #(if (pred %) %) coll))
