(defproject tailrecursion/javelin "3.7.2"
  :description "A Functional Reactive Programming library for ClojureScript"
  :url "https://github.com/tailrecursion/javelin"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[tailrecursion/cljs-priority-map "1.0.3"]
                 [org.clojure/data.priority-map   "0.0.2"]
                 [riddley                         "0.1.6"]]
  :deploy-repositories [["clojars" {:url "https://clojars.org/repo" :sign-releases false :creds :gpg}]]
  :source-paths ["src"]
  :repl-options {:init-ns tailrecursion.javelin}
  :profiles {:devz {:plugins [[lein-cljsbuild "0.3.2"]
                              [com.cemerick/clojurescript.test "0.1.0"]]
                    :dependencies [[org.clojure/clojure "1.6.0"]
                                   [org.clojure/clojurescript "0.0-2311"]]}} 
  :cljsbuild {:builds
              {:dev-test
               {:source-paths ["src" "test"]
                :compiler {:output-to "test/test.js"
                           :optimizations :whitespace
                           :warnings true}
                :jar false}
               :test
               {:source-paths ["src" "test"]
                :compiler {:output-to "test/test.js"
                           :optimizations :advanced
                           :warnings true}
                :jar false}}})
