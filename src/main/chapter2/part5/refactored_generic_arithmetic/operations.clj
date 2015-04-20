(ns chapter2.part5.refactored-generic-arithmetic.operations)

(defn add [x y] (apply-generic :add x y))
(defn sub [x y] (apply-generic :sub x y))
(defn mul [x y] (apply-generic :mul x y))
(defn div [x y] (apply-generic :div x y))

;; equ?
;; zero?
