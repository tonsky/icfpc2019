(ns icfpc.main
  (:require
   [icfpc.bot :as bot]
   [icfpc.level :as level]
   [clojure.string :as str]
   [clojure.java.io :as io]))

(def log-agent (agent nil))

(defn log [& msg] (send log-agent (fn [_] (apply println msg) _)))

(defn compare-solutions [name score]
  (->> (file-seq (io/file "."))
    (filter #(str/starts-with? (.getName %) "day"))
    (sort-by #(.getName %))
    (map #(io/file % (str name ".sol")))
    (filter #(.exists %))
    (map #(bot/path-score (slurp %)))
    (map #(format "%d (%+.1f%%)" % (-> (- score %) (/ %) (* 100) (float))))))

(defn solve [name & [opts]]
  (try
    ; (log "Solving" name "...")
    (let [level (level/load-level (str name ".desc"))
          sln   (bot/solve level (merge
                                   {:debug? false
                                    :lookahead? (<= (:width level) 200)}
                                   opts))]

      (log (when-some [t0 (:t0 opts)]
             (str (- (System/currentTimeMillis) t0) "ms"))
           "Solved" name (dissoc sln :path) "was" (str/join " / " (compare-solutions name (:score sln))))
      (spit (str "problems/" name ".sol") (:path sln)))
    (catch Exception e
      (.printStackTrace e))))

(defn skip-till [n xs]
  (if (some? n) (drop n xs) xs))

(defn take-till [to xs]
  (if (some? to) (take to xs) xs))

(defn -main [& [from till]]
  (let [from  (cond-> from (string? from) (Integer/parseInt))
        till  (cond-> till (string? till) (Integer/parseInt))
        names (->> (file-seq (io/file "problems"))
                 (map #(.getPath %))
                 (filter #(str/ends-with? % ".desc"))
                 (keep #(second (re-matches #".*/(prob-\d\d\d)\.desc" %)))
                 sort
                 (take-till till)
                 (skip-till from))
        t0       (System/currentTimeMillis)
        threads  (.. Runtime getRuntime availableProcessors)
        executor (java.util.concurrent.Executors/newFixedThreadPool threads)]
    (log "Running" threads "threads")
    (doseq [name names]
      (.submit executor ^Callable
        (fn [] (solve name {:t0 t0}))))))

(defn print-solve [name]
  (bot/print-level (level/load-level (str name ".desc")))
  (solve name))

(comment
  (icfpc.main/solve "prob-010")
  (icfpc.main/print-solve "prob-002")
  (icfpc.bot/solve (icfpc.level/load-level "prob-010.desc") {:delay 100})
  (icfpc.main/score-solutions "problems")
)

(defn score-solutions [path]
  (->> (file-seq (io/file path))
       (filter #(str/ends-with? (.getName %) ".sol"))
       (map slurp)
       (map bot/path-score)
       (reduce + 0)))