(ns chapter2.part5.refactored-generic-arithmetic.operations
  (:require [chapter2.part5.refactored-generic-arithmetic.common :as common]
            [chapter2.part5.refactored-generic-arithmetic.tables :as table]))

(defn- apply-generic [operation & args]
  (let [type-tags (map common/type-tag args)
        proc (table/get operation type-tags)]
    (if proc
      (apply proc (map common/contents args))
      (common/error "No method for these types: APPLY-GENERIC" (list operation args)))))

(defn add [x y] (apply-generic :add x y))
(defn sub [x y] (apply-generic :sub x y))
(defn mul [x y] (apply-generic :mul x y))
(defn div [x y] (apply-generic :div x y))
(defn zero?- [x] (apply-generic :zero x))
(defn equ? [x y] (apply-generic :equ x y))
