(defproject icfpc "2019.0.0"
  :dependencies
  [[org.clojure/clojure   "1.9.0"]
   [org.clojure/data.json "0.2.6"]]
  :source-paths   ["src"]
  :resource-paths ["resources"]
  :main icfpc.main
  :jvm-opts ["-Xverify:none"]
  :plugins [[io.taylorwood/lein-native-image "0.3.0"]]  

  :native-image {:name "icfpc2019"
                 :opts ["--report-unsupported-elements-at-runtime"
                        "--initialize-at-build-time"
                        "--no-fallback"
                        "-H:ReflectionConfigurationFiles=reflectconfig.json"]
                 :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}
  :profiles {:uberjar {:aot :all}})