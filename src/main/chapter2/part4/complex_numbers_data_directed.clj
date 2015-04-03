(ns chapter2.part4.complex-numbers-data-directed
  (:require [chapter2.part4.operations-table :as table]
            [chapter2.part4.common-stuff :as common]
            [chapter2.part2.samples :refer [car cdr map- cadr length]]))

(defn apply-generic [operation & args]
  (let [type-tags (map- common/type-tag args)
        proc (table/get operation type-tags)]
    (if proc
      (apply proc (map- common/contents args))
      (common/error "No method for these types: APPLY-GENERIC" (list operation args)))))

(defn real-part [z]
  (apply-generic :real-part z))

(defn imag-part [z]
  (apply-generic :imag-part z))

(defn magnitude [z]
  (apply-generic :magnitude z))

(defn angle [z]
  (apply-generic :angle z))

(defn make-from-real-imag [x y]
  ((table/get :make-from-real-imag :rectangular) x y))
(defn make-from-mag-ang [r a]
  ((table/get :make-from-mag-ang :polar) r a))

(defn apply-generic-coercion [operation & args]
  (let [type-tags (map- common/type-tag args)
        proc (table/get operation type-tags)]
    (if proc
      (apply proc (map- common/contents args))
      (if (= (length args) 2)
        (let [type1 (car type-tags)
              type2 (cadr type-tags)
              a1 (car args)
              a2 (cadr args)
              t1->t2 (table/get-coercion type1 type2)
              t2->t1 (table/get-coercion type2 type1)]
          (cond t1->t2 (apply-generic-coercion operation (t1->t2 a1) a2)
                t2->t1 (apply-generic-coercion operation a1 (t2->t1 a2))
                :else (common/error "No method for these types" (list operation type-tags))))))))
