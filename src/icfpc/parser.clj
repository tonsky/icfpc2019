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
  (let [description (slurp (str "resources/part-1-initial/" task-name))
        [_ level start obstacles busters] (re-matches #"(.*)#(.*)#(.*)#(.*)" description)]
    {:bot-point     (first (parse-points start))
     :corners       (parse-points level)
     :obstacles     (mapv parse-points (filter not-empty (split obstacles #";")))
     :busters       (mapv parse-buster (filter not-empty (split busters #";")))}))
