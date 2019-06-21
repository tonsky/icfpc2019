(ns user
  (:require
   [cljs.stacktrace]
   [figwheel.main.api :as fig]
   [com.stuartsierra.component :as component]
   [clojure.tools.namespace.repl :as namespace]))

(namespace/disable-reload!)
(namespace/set-refresh-dirs "src" "dev")

(defonce *system (atom nil))

(defn stop []
  (some-> @*system (component/stop))
  (reset! *system nil))

(defn reload []
  (let [res (namespace/refresh)]
    (when (not= res :ok)
      (throw res))
    res))

(defn start
  ([] (start {:server {:join? false}}))
  ([opts]
   (some->>
     (some->
       (resolve 'icfpc.main/system)
       (.invoke opts)
       (component/start))
     (reset! *system))))

(defn reset []
  (stop)
  (reload)
  (start))

(defn figwheel []
  (fig/start
    {:mode               :serve
     :rebel-readline     false
     :cljs-devtools      false
     :helpful-classpaths false
     :open-url           false}
    {:id      "dev"
     :config  {:watch-dirs ["src"]
               :css-dirs   ["resources/static"]}
     :options {:main       'icfpc.app
               :output-to  "target/resources/static/app.js"
               :output-dir "target/resources/static/app"
               :asset-path "/static/app"}}))