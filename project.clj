(defproject cljgfa "0.1.0-cljgfa"
  :description "Clojure GFA parser built on instaparse"
  :url ""
  :license {:name "GPL3"
            :url "https://www.gnu.org/licenses/gpl-3.0.en.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot cljgfa.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
