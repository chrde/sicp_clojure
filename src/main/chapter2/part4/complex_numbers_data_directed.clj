(ns chapter2.part4.complex-numbers-data-directed
  (:require [chapter2.part4.operations-table :as table]
            [chapter2.part4.common-stuff :as common]
            [chapter2.part2.samples :as ch2]))

(defn apply-generic [operation & args]
  (let [type-tags (ch2/map- common/type-tag args)
        proc (table/get operation type-tags)]
    (if proc
      (apply proc (ch2/map- common/contents args))
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

