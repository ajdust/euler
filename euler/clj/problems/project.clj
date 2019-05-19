(defproject problems "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "https://github.com/johnsabr/euler/"
  :license {:name "MIT"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.9.0"] [org.clojure/math.numeric-tower "0.0.4"]]
  :main ^:skip-aot problems.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
