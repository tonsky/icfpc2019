(ns icfpc.main
  (:require
   [icfpc.bot :as bot]
   [icfpc.level :as level]
   [clojure.string :as str]
   [clojure.java.io :as io]
   [clojure.java.shell :as shell]
   [clojure.data.json :as json])
  (:import
   [java.util.concurrent CompletableFuture]))

(set! *warn-on-reflection* true)

(def log-agent (agent nil))

(defn log [& msg] (send log-agent (fn [_] (apply println msg) _)))

(defn compare-solutions [name score]
  (->> (file-seq (io/file "."))
    (filter #(str/starts-with? (.getName %) "day"))
    (sort-by #(.getName %))
    (map #(io/file % (str name ".sol")))
    (filter #(.exists %))
    (map #(bot/path-score (slurp %)))
    (distinct)
    (map #(format "%d (%+.1f%%)" % (-> (- score %) (/ %) (* 100) (float))))))

(defn solve [name & [opts]]
  (try
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
      (println (.getMessage e)))))

(defn skip-till [n xs]
  (if (some? n) (drop n xs) xs))

(defn take-till [to xs]
  (if (some? to) (take to xs) xs))

(defn -main [& [from till threads]]
  (let [from  (cond-> from (string? from) (Integer/parseInt))
        till  (cond-> till (string? till) (Integer/parseInt))
        names (->> (file-seq (io/file "problems"))
                 (map #(.getPath %))
                 (filter #(str/ends-with? % ".desc"))
                 (keep #(second (re-matches #".*/(prob-\d\d\d)\.desc" %)))
                 sort
                 (take-till till)
                 (skip-till from)
                 (remove #(.exists (io/file (str "problems/" % ".sol")))))
        t0       (System/currentTimeMillis)
        threads  (or (cond-> threads (string? threads) (Integer/parseInt))
                   (.. Runtime getRuntime availableProcessors))
        executor (java.util.concurrent.Executors/newFixedThreadPool threads)]
    (log "Running" threads "threads")
    (->
      (into-array CompletableFuture
        (for [name names]
          (CompletableFuture/runAsync
            ^Runnable (fn [] (solve name {:t0 t0}))
            executor)))
      (CompletableFuture/allOf)
      (.join))
    (log "DONE in" (- (System/currentTimeMillis) t0) " ms")
    (.shutdown executor)))

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

(defn mine-coins [& [block excluded puzzle task]]
  (spit "mine.log" (str block excluded))
  (prn block excluded puzzle task))

(defn run-lambda [& commands]
  (let [result (shell/with-sh-dir "client/lambda-client"
                                  (apply shell/sh
                                         "./lambda-cli.py"
                                         commands))]
    (when (not= 0 (:exit result))
      (prn "Err: " (:err result)))
    (:out result)))

(defn mine-block [block]
  (let [block (Integer/parseInt (clojure.string/trim block))
        puzzle (run-lambda "getblockinfo" (str block) "puzzle")
        task (run-lambda "getblockinfo" (str block) "task")
        puzzle-name (format "puzzle-%03d.cond" block)
        task-name (format "task-%03d.desc" block)
        solution-name (format "solution-%03d.sol" block)
        generated-name (format "generated-%03d.desc" block)]
    (prn (format "Round: %03d" block))
    (spit (str "./puzzles/" puzzle-name) puzzle)
    (spit (str "./puzzles/" task-name) task)
    (prn "Generate level...")
    (let [puzzle (icfpc.parser/parse-puzzle (str "../puzzles/" puzzle-name))
          level (icfpc.level/generate-level (str "../puzzles/" puzzle-name))]
      (if (icfpc.parser/validate-puzzle puzzle level)
        (do
          (spit (str "./puzzles/" generated-name) (icfpc.writer/desc level))
          (prn "Level generated...")
          (prn "Trying to solve level...")
          (let [level (level/load-level (str "../puzzles/" task-name))
                sln   (bot/solve level (merge
                                        {:debug? false
                                         :lookahead? false #_(<= (:width level) 200)}))]
            (prn "Solved " (dissoc sln :path))
            (spit (str "./puzzles/" solution-name) (:path sln))))
        (prn "Level generation failed.")))
    (shutdown-agents)))

(defn mine []
  (let [block (Integer/parseInt (clojure.string/trim (run-lambda "getmininginfo" "block")))
        puzzle (run-lambda "getmininginfo" "puzzle")
        task (run-lambda "getmininginfo" "task")
        puzzle-name (format "puzzle-%03d.cond" block)
        task-name (format "task-%03d.desc" block)
        solution-name (format "solution-%03d.sol" block)
        generated-name (format "generated-%03d.desc" block)]
    (prn (format "Round: %03d" block))
    (spit (str "./puzzles/" puzzle-name) puzzle)
    (spit (str "./puzzles/" task-name) task)
    (prn "Generate level...")
    (let [puzzle (icfpc.parser/parse-puzzle (str "../puzzles/" puzzle-name))
          level (icfpc.level/generate-level (str "../puzzles/" puzzle-name))]
      (if (icfpc.parser/validate-puzzle puzzle level)
        (do
          (spit (str "./puzzles/" generated-name) (icfpc.writer/desc level))
          (prn "Level generated...")
          (prn "Trying to solve level...")
          (let [level (level/load-level (str "../puzzles/" task-name))
                sln   (bot/solve level (merge
                                        {:debug? false
                                         :lookahead? false #_(<= (:width level) 200)}))]
            (prn "Solved " (dissoc sln :path))
            (spit (str "./puzzles/" solution-name) (:path sln)))
          (prn (run-lambda "submit" (str block) (str "../../puzzles/" solution-name) (str "../../puzzles/" generated-name))))
        (prn "Level generation failed.")))
    (shutdown-agents)))


(comment

  (run-lambda "")
  (def block (Integer/parseInt (clojure.string/trim (run-lambda "getmininginfo" "block"))))
  (mine)

  (spit "./puzzles/foo.txt" "hello")
  (spit "./puzzles/foo.txt" "world")

  (mine)
  *e

  )
