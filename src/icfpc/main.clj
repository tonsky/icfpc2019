(ns icfpc.main
  (:require
   [icfpc.bot :as bot]
   [icfpc.level :as level]
   [clojure.string :as str]
   [clojure.java.io :as io]))

(defn solve [name & [opts]]
  (try
    (println "Solving" name "...")
    (let [level (level/load-level (str name ".desc"))
          sln   (bot/solve level (merge
                                   {:debug? false
                                    :lookahead? (<= (:level/width level) 200)}
                                   opts))]
      (println "Solved" name (dissoc sln :path))
      (spit (str "problems/" name ".sol") (:path sln)))
    (catch Exception e
      (.printStackTrace e))))

(defn skip-till [n xs]
  (cond
    (nil? n) xs
    (string? n) (drop (Integer/parseInt n) xs)
    (number? n) (drop n xs)))

(defn -main [& [skip]]
  (let [names (->> (file-seq (io/file "problems"))
                 (map #(.getPath %))
                 (filter #(str/ends-with? % ".desc"))
                 (map #(second (re-matches #".*/(prob-\d\d\d)\.desc" %)))
                 sort
                 (skip-till skip))]
    (doall (pmap solve names))))

(defn print-solve [name]
  (bot/print-level (level/load-level (str name ".desc")))
  (solve name))

(comment
  (icfpc.main/solve "prob-010")
  (icfpc.main/print-solve "prob-002")
  (icfpc.bot/solve (icfpc.level/load-level "prob-010.desc") {:delay 100})
)