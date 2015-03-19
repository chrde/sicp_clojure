(ns chapter2.part3.exercises
  (:require [chapter2.part2.samples :as ch2]
            [chapter2.part3.samples :refer :all]
            [chapter1.samples :as ch1]))

;; 2.54
(defn equal? [x y]
  (if (and (coll? x) (coll? y))
    (and (equal? (ch2/car x) (ch2/car y))
         (equal? (ch2/cdr x) (ch2/cdr y)))
    (= x y)))

;; 2.56
(defn base [s]
  (ch2/cadr s))

(defn exponent [s]
  (ch2/caddr s))

(defn make-exponentiation [base exponent]
  (list '** base exponent))

(defn make-exponentiation-simplest [base exponent]
  (cond (=number? exponent 0) 1
        (=number? exponent 1) base
        (and (number? base) (number? exponent)) (ch1/recur-fast-expt base exponent)
        :else (make-exponentiation base exponent)))

(defn exponentiation? [x]
  (and (coll? x) (eq? (ch2/car x) '**)))

(defn deriv-exp [exp var]
  (cond (number? exp) 0
        (variable? exp) (if (same-variable? exp var) 1 0)
        (sum? exp)
        (make-sum-simplest (deriv-exp (addend exp) var)
                           (deriv-exp (augend exp) var))
        (product? exp)
        (make-sum-simplest (make-product-simplest (multiplier exp)
                                                  (deriv-exp (multiplicand exp) var))
                           (make-product-simplest (deriv-exp (multiplier exp) var)
                                                  (multiplicand exp)))
        (exponentiation? exp)
        (make-product-simplest (make-product-simplest (exponent exp)
                                                      (make-exponentiation-simplest
                                                        (base exp)
                                                        (make-sum-simplest (exponent exp) -1)))
                               (deriv-exp (base exp) var))

        :else (throw (Exception. (str "Unknown expression type: DERIV-EXP" exp)))))
