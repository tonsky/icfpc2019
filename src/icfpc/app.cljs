(ns ^:figwheel-hooks icfpc.app
  (:require
   [goog.net.XhrIo]
   [rum.core :as rum]
   [clojure.string :as str]
   [icfpc.transit :as transit]))

(enable-console-print!)

(defn ajax! [method url {:keys [on-success on-error content headers]}]
  (.send goog.net.XhrIo url
    (fn [reply]
      (let [xhr    (.-target reply)
            status (.getStatus xhr)
            text   (.getResponseText xhr)]
        (if (<= 200 status 299)
          (when on-success
            (on-success (when-not (str/blank? text) (transit/read-string text))))
          (when on-error
            (on-error status text)))))
    method
    (or (some-> content transit/write-string) "")
    (clj->js headers)))

;; (core/ajax! "GET" "/api/endpoint"
;;   {:on-success (fn [data] (println data))
;;    :on-erorr   (fn [status text] (println status text))
;;    :headers    {"Content-Type" "application/transit+json"}})

(rum/defcs app
  < (rum/local nil ::*time)
    {:will-mount
     (fn [state]
       (let [{::keys [*time]} state]
         (ajax! "GET" "/api/time" {:on-success #(reset! *time (:time %))}))
       state)}
  [{::keys [*time]} state]
  [:h1 "Hello ICFPC! " (pr-str @*time)])

(defn ^:after-load ^:export refresh []
  (let [mount (js/document.querySelector "#mount")]
    (rum/mount (app) mount)))