(ns icfpc.main
  (:require
   [icfpc.bot :as bot]
   [icfpc.level :as level]
   [clojure.string :as str]
   [clojure.java.io :as io]))

(defn -main [& args]
  (doseq [path (->> (file-seq (io/file "resources/part-1-initial"))
                 (map #(.getPath %))
                 (filter #(str/ends-with? % ".desc"))
                 sort)]
    (try
      (let [[_ name] (re-matches #".*/(prob-\d\d\d)\.desc" path)
            _        (println "Solving" name)
            level    level/prob-001
            sln      (bot/solve level false)]
        (spit (str "resources/solutions/" name ".sol") sln))
      (catch Exception e
        (.printStackTrace e)))))