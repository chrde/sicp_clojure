(ns chapter2.part5.refactored-generic-arithmetic.tables)

(def table (atom {}))

(defn put [operation type item]
  (swap! table assoc-in [operation type] item))

(defn get [operation type]
  (get-in @table [operation type]))

(def coercion-table (atom {}))

(defn put-coercion [operation type1 type2]
  (swap! coercion-table assoc-in [type1 type2] operation))

(defn get-coercion [type1 type2]
  (get-in @coercion-table [type1 type2]))
