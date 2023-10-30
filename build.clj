(ns build
  (:refer-clojure :exclude [test])
  (:require [clojure.string :as str]
            [clojure.tools.build.api :as b]
            [clojure.tools.deps :as t]
            [deps-deploy.deps-deploy :as dd]))

(def lib 'hoplon/javelin)
(def version "3.9.1")
(def snapshot (format "%s-SNAPSHOT" version))
#_ ; alternatively, use MAJOR.MINOR.COMMITS:
(def version (format "1.0.%s" (b/git-count-revs nil)))
(def class-dir "target/classes")

(defn- pom-template [version]
  [[:description "Spreadsheet-like dataflow programming in ClojureScript."]
   [:url "https://github.com/hoplon/javelin"]
   [:licenses
    [:license
     [:name "Eclipse Public License"]
     [:url "http://www.eclipse.org/legal/epl-v10.html"]]]
   [:developers
    [:developer
     [:name "Alan Dipert"]]
    [:developer
     [:name "Micha Niskin"]]]
   [:scm
    [:url "https://github.com/hoplon/javelin"]
    [:connection "scm:git:https://github.com/hoplon/javelin.git"]
    [:developerConnection "scm:git:ssh:git@github.com:hoplon/javelin.git"]
    [:tag (str "v" version)]]])

(defn- run-task [aliases]
  (println "\nRunning task for" (str/join "," (map name aliases)))
  (let [basis    (b/create-basis {:aliases aliases})
        combined (t/combine-aliases basis aliases)
        cmds     (b/java-command
                  {:basis      basis
                   :main      'clojure.main
                   :main-args (:main-opts combined)})
        {:keys [exit]} (b/process cmds)]
    (when-not (zero? exit) (throw (ex-info "Task failed" {})))))

(defn node-test
  "Run cljs tests using node"
  [opts]
  (run-task [:cljs-node])
  opts)

(defn chrome-test
  "Run cljs tests using chrome"
  [opts]
  (run-task [:cljs-chrome])
  opts)

(defn clojure-test
  "Run cljs tests using chrome"
  [opts]
  (run-task [:test :runner])
  opts)

(defn test
  "Run all the tests."
  [opts]
  (clojure-test opts)
  (node-test opts)
  (chrome-test opts)
  opts)

(defn- jar-opts [opts]
  (let [version (if (:snapshot opts) snapshot version)]
    (println "\nVersion:" version)
    (assoc opts
      :lib lib :version version
      :jar-file (format "target/%s-%s.jar" lib version)
      :basis (b/create-basis {})
      :class-dir class-dir
      :target "target"
      :src-dirs ["src" "clj-kondo"]
      :pom-data (pom-template version))))

(defn ci "Run the CI pipeline of tests (and build the JAR)." [opts]
  (test opts)
  (b/delete {:path "target"})
  (let [opts (jar-opts opts)]
    (println "\nWriting pom.xml...")
    (b/write-pom opts)
    (println "\nCopying source...")
    (b/copy-dir {:src-dirs ["src" "clj-kondo"] :target-dir class-dir})
    (println "\nBuilding JAR...")
    (b/jar opts))
  opts)

(defn install "Install the JAR locally." [opts]
  (let [opts (jar-opts opts)]
    (b/install opts))
  opts)

(defn deploy "Deploy the JAR to Clojars." [opts]
  (let [{:keys [jar-file] :as opts} (jar-opts opts)]
    (dd/deploy {:installer :remote :artifact (b/resolve-path jar-file)
                :pom-file (b/pom-path (select-keys opts [:lib :class-dir]))}))
  opts)
