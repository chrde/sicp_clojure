(ns chapter2.part4.operations-table)

(def table (atom {}))

(defn put [operation type item]
  (swap! table assoc-in [operation type] item))

(defn get [operation type]
  (get-in @table [operation type]))
