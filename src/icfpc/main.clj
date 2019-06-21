(ns icfpc.main
  (:require
   [icfpc.routes :as routes]
   [ring.adapter.jetty :as jetty]
   [com.stuartsierra.component :as component]))

(defrecord Server [opts jetty]
  component/Lifecycle
  (start [this]
    (when (nil? jetty)
      (println "Starting web server at" (str (:host opts) ":" (:port opts)))
      (assoc this :jetty
        (jetty/run-jetty #'routes/routes opts))))
  (stop [this]
    (when (some? jetty)
      (println "Stopping web server")
      (.stop jetty)
      (assoc this :jetty nil))))

(defn server [opts]
  (->Server (merge {:host "localhost" :port 8080} opts) nil))

(defn system [opts]
  (component/system-map
    :server (server (:server opts))))

(defn -main [& args]
  (-> (system {})
    (component/start)))