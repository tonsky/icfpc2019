(ns icfpc.transit
  (:refer-clojure :exclude [read read-string write])
  (:require
   [cognitect.transit :as t])
  (:import
   [java.io ByteArrayInputStream ByteArrayOutputStream]))
 
(defn read [is]
  (t/read (t/reader is :json)))

(defn read-string [^String s]
  (read (ByteArrayInputStream. (.getBytes s "UTF-8"))))

(defn write [o os]
  (t/write (t/writer os :json) o))

(defn write-bytes ^bytes [o]
  (let [os (ByteArrayOutputStream.)]
    (write o os)
    (.toByteArray os)))
    
(defn write-string [o]
  (String. (write-bytes o) "UTF-8"))