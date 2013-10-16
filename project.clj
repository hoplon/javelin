(defproject tailrecursion/javelin "2.0.0"
  :description "A Functional Reactive Programming library for ClojureScript"
  :url "https://github.com/tailrecursion/javelin"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[tailrecursion/cljs-priority-map "1.0.2"]
                 [alandipert/desiderata "1.0.1"]]
  :plugins [[lein-cljsbuild "0.3.2"]
            [com.cemerick/clojurescript.test "0.1.0"]]
  :source-paths ["src"]
  :repl-options {:init-ns tailrecursion.javelin}
  :cljsbuild {:builds
              {:test
               {:source-paths ["src" "test"]
                :compiler {:output-to "test/test.js"
                           :optimizations :advanced
                           ;:optimizations :whitespace
                           ;:pretty-print true
                           :warnings true}
                :jar false}}})
