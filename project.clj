(defproject icfpc "2019.0.0"
  :dependencies
  [[org.clojure/clojure         "1.10.1"]
   [org.clojure/tools.namespace "0.2.11"]
   [org.clojure/data.json       "0.2.6"]]
  :source-paths   ["src"]
  :resource-paths ["resources"]
  :main           icfpc.main
  :jvm-opts ["-Xverify:none"]
  
  :profiles {
    :dev {
      :source-paths ["dev"]
    }
  })