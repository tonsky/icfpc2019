(ns icfpc.main
  (:require
   [icfpc.bot :as bot]
   [icfpc.level :as level]
   [clojure.string :as str]
   [clojure.java.io :as io]))

(defn solve [name]
  (print "Solving" name "...")
  (flush)
  (let [level (level/load-level (str name ".desc"))
        sln   (bot/solve level {:debug? false})]
    (println " found" sln)
    (spit (str "resources/solutions/" name ".sol") sln)))

(defn skip-till [n xs]
  (cond
    (nil? n) xs
    (string? n) (drop (Integer/parseInt n) xs)
    (number? n) (drop n xs)))

(defn -main [& [skip]]
  (doseq [name (->> (file-seq (io/file "resources/part-1-initial"))
                 (map #(.getPath %))
                 (filter #(str/ends-with? % ".desc"))
                 (map #(second (re-matches #".*/(prob-\d\d\d)\.desc" %)))
                 sort
                 (skip-till skip))]
    (try
      (solve name)
      (catch Exception e
        (.printStackTrace e)))))

(defn print-solve [name]
  (bot/print-level (level/load-level (str name ".desc")))
  (solve name))

(comment
  (icfpc.main/solve "prob-010")
  (icfpc.main/print-solve "prob-002")
  (icfpc.bot/solve (icfpc.level/load-level "prob-010.desc") {:delay 100})
)