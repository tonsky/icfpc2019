(ns icfpc.routes
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]
   [icfpc.transit :as transit]
   [ring.util.response :as response]))

(defn transit-response [data]
  {:status  200
   :headers {"Content-Type" "application/transit+json"}
   :body    (transit/write-string data)})

(defn routes [{:keys [request-method uri] :as req}]
  (case request-method
    :get
    (condp re-matches uri
      #"/"                (response/resource-response "static/index.html")
      #"/api/time"        (transit-response {:time (java.util.Date.)})
      #"/static/(.*)" :>> (fn [[_ path]] (response/resource-response (str "static/" path)))
      {:status 404 :body "404 Not Found"})

    {:status 404 :body "404 Not Found"}))
