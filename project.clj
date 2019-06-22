(defproject icfpc "2019.0.0"
  :dependencies
  [[org.clojure/clojure         "1.10.1"]
   [org.clojure/tools.namespace "0.2.11"]]
  :source-paths   ["src"]
  :resource-paths ["resources"]
  :main           icfpc.main
  
  :profiles {
    :dev {
      :source-paths ["dev"]
    }
  })