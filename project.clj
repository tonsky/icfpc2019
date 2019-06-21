(defproject icfpc "2019.0.0"
  :dependencies
  [[org.clojure/clojure         "1.10.1"]
   [org.clojure/clojurescript   "1.10.520"]
   [com.bhauman/figwheel-main   "0.2.0"]
   [com.stuartsierra/component  "0.4.0"]
   [org.clojure/tools.namespace "0.2.11"]
   [ring/ring-core              "1.6.3"]
   [com.cognitect/transit-cljs  "0.8.256"]
   [com.cognitect/transit-clj   "0.8.313"]
   [rum                         "0.11.3"]]
  :source-paths   ["src"]
  :resource-paths ["resources"]
  :main           icfpc.main
  
  :profiles {
    :dev {
      :source-paths   ["dev"]
      :resource-paths ["target/resources"]
      :repl-options   {:init-ns user}
    }
  })