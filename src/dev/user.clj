(ns dev.user
  (:require [clojure.repl :refer :all]
            [clojure.tools.namespace.repl :refer (refresh refresh-all)]))

(defn reset []
  (println "Refreshing the REPL...")
  (refresh))
