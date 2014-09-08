(defproject hello-world "0.1.0-SNAPSHOT"
  :plugins [[lein-cljsbuild "0.3.2"]]
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-1806"]]
  :source-paths ["src/cljs"]
  :cljsbuild {
    :builds [{
      :source-path "src/cljs"
      :compiler {
        :output-to "resources/public/hello.js"
        :optimizations :whitespace
        :pretty-print true}}]})
