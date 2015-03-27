(defproject sicp_clojure "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.6.0"]]
  :profiles {:dev {:source-paths ["src/dev"]
                   :dependencies [[org.clojure/tools.namespace "0.2.10"]
                                  [org.clojure/java.classpath "0.2.2"]]}}
  :source-paths ["src" "src/main"])
