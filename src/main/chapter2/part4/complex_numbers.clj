(ns chapter2.part4.complex-numbers
  (:require [chapter2.part4.operations-table :as table]
            [chapter2.part4.common-stuff :as common]
            [chapter2.part4.complex-numbers-data-directed :as p4]
            [chapter2.part3.symbolic-differentiation-samples :as p3]
            [chapter2.part2.samples :as p2]))

;; 2.73
(defn install-deriv-package []
  (letfn [(deriv-sum [exp var]
            (p3/make-sum (p3/deriv (p3/addend exp) var)
                         (p3/deriv (p3/augend exp) var)))
          (deriv-mult [exp var]
            (p3/make-sum (p3/make-product (p3/multiplier exp)
                                          (p3/deriv (p3/multiplicand exp) var))
                         (p3/make-product (p3/deriv (p3/multiplier exp) var)
                                          (p3/multiplicand exp))))]
    (do (table/put :deriv '+ (fn [x y] (common/attach-tag '+ (deriv-sum x y))))
        (table/put :deriv '* deriv-mult))))

(install-deriv-package)

(defn operator [exp]
  (p2/car exp))

(defn operands [exp]
  (p2/cdr exp))

(defn deriv [exp var]
  (cond (number? exp) 0
        (p3/variable? exp) (if (p3/same-variable? exp var) 1 0)
        :else ((table/get :deriv (operator exp))
               (operands exp) var)))
;; 2.74
;; 2.75
;; 2.76
; When something changes, it would be nice to make changes in just one place
; With this in mind, I would say
; When new types are often added, message-passing-style is better
; - Because the entire type definition can be seen as one module in the source code
; When new operations are often added, data-directed-style is better
; - Because the operation (for all the types) can be kept in one module as a whole

; These are reasons from a developing/maintaining point of view, since ultimately both
; options do "the same"
