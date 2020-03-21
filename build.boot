(set-env!
  :dependencies   '[[org.clojure/clojure             "1.9.0"      :scope "provided"]
                    [org.clojure/clojurescript       "1.9.293"    :scope "provided"]
                    [adzerk/bootlaces                "0.1.10"     :scope "test"]
                    [adzerk/boot-cljs                "1.7.228-2"  :scope "test"]
                    [riddley                         "0.2.0"]]
  :resource-paths #{"src"})

(require
  '[clojure.test :as t]
  '[clojure.java.io  :as io]
  '[adzerk.bootlaces :refer :all]
  '[adzerk.boot-cljs :refer :all])

(def +version+ "3.10.0")

(bootlaces! +version+)

(alter-var-root #'build-jar #(fn [] (comp (javac) (%))))

(task-options!
  pom  {:project     'hoplon/javelin
        :version     +version+
        :description "Spreadsheet-like dataflow programming in ClojureScript"
        :url         "https://github.com/hoplon/javelin"
        :scm         {:url "https://github.com/hoplon/javelin"}
        :license     {"Eclipse Public License" "http://www.eclipse.org/legal/epl-v10.html"}})

(deftask test-runner
  []
  (let [tmp (tmp-dir!)]
    (with-pre-wrap fileset
      (with-let [fs fileset]
        (empty-dir! tmp)
        (doseq [[path in-file] (map (juxt tmp-path tmp-file) (output-files fs))]
          (let [out-file (doto (io/file tmp path) io/make-parents)]
            (io/copy in-file out-file)))
        (binding [*sh-dir* (.getPath tmp)] (dosh "bash" "run.sh"))))))

(deftask test-javelin
  "Run Javelin tests"
  [a advanced bool "Test with :advanced optimizations."]
  (merge-env! :resource-paths #{"test"})
  (when advanced (task-options! cljs {:optimizations :advanced}))
  (comp (cljs)
        (test-runner)
        (test-runner)
        (test-runner)
        (test-runner)
        (test-runner)
        (test-runner)
        (test-runner)
        (test-runner)
        (test-runner)
        (test-runner)))

(deftask test-javelin-clj
  "Run Javelin-CLJ tests"
  []
  (merge-env! :resource-paths #{"test"})
  (comp (javac)
        (with-pass-thru [_]
          (require 'javelin.cell-test :reload)
          (time (t/run-tests 'javelin.cell-test)))))
