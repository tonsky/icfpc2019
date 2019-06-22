(ns icfpc.parser
  (:require [clojure.string :refer [split]]))

(defn parse-points [points-str]
  (if (not-empty points-str)
    (let [points-seq (split (->> points-str
                                 (drop 1)
                                 (drop-last 1)
                                 (apply str))
                            #"\),\(")]
      (mapv (fn [p]
              (mapv #(Integer/parseInt %) (split p #",")))
            points-seq))
    []))

(defn parse-buster [buster-str]
  (let [[type point-str] (split-at 1 buster-str)]
    [(first type) (first (parse-points point-str))]))

(defn parse-level [task-name]
  (let [description (slurp (str "problems/" task-name))
        [_ level start obstacles busters] (re-matches #"(.*)#(.*)#(.*)#(.*)" description)]
    {:bot-point (first (parse-points start))
     :corners   (parse-points level)
     :obstacles (mapv parse-points (filter not-empty (split obstacles #";")))
     :boosters  (mapv parse-buster (filter not-empty (split busters #";")))}))

(defn parse-puzzle [puzzle-name]
  (let [description (slurp (str "puzzles/" puzzle-name))
        [_ params include exclude] (re-matches #"(.*)#(.*)#(.*)" description)
        [block-number epoch t-size v-min v-max extra-hands fast-wheels drills teleports cloning spawns] (split params #",")]
    {:block-number block-number
     :epoch epoch
     :t-size t-size
     :v-min v-min
     :v-max v-max
     :extra-hands extra-hands
     :fast-wheels fast-wheels
     :drills drills
     :teleports teleports
     :cloning cloning
     :spawns spawns
     :include (parse-points include)
     :exclude (parse-points exclude)
     }))

(comment

  )