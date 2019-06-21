(ns icfpc.transit
  (:refer-clojure :exclude [read read-string write])
  (:require
   [cognitect.transit :as t]))

(defn read-string [s]
  (t/read (t/reader :json) s))

(defn write-string [o]
  (t/write (t/writer :json) o))
