(ns chapter2.part3.samples
  (:require [chapter2.part2.samples :as ch2]
            [chapter2.part3.exercises :as ch3]))

(def eq? =)

(defn memq [item coll]
  (cond (nil? coll) false
        (eq? item (ch2/car coll)) coll
        :else (memq item (ch2/cdr coll))))

(defn variable? [x]
  (symbol? x))

(defn same-variable? [x y]
  (and (variable? x)
       (variable? y)
       (= x y)))

(defn make-sum [x y]
  (list '+ x y))

(defn make-product [x y]
  (list '* x y))

(defn sum? [x]
  (and (coll? x) (eq? (ch2/car x) '+)))

(defn addend [s]
  (ch2/cadr s))

(defn augend [s]
  (ch2/car (ch2/cdr (ch2/cdr s))))

(defn product? [x]
  (and (coll? x) (eq? (ch2/car x) '*)))

(defn multiplier [s]
  (ch2/cadr s))

(defn multiplicand [s]
  (ch2/caddr s))

(defn =number? [exp num]
  (and (number? exp) (= exp num)))

(defn make-sum-simplest [x y]
  (cond (=number? x 0) y
        (=number? y 0) x
        (and (number? x) (number? y)) (+ x y)
        :else (make-sum x y)))

(defn make-product-simplest [x y]
  (cond (or (=number? x 0) (=number? y 0)) 0
        (=number? x 1) y
        (=number? y 1) x
        (and (number? x) (number? y)) (* x y)
        :else (make-product x y)))

(defn deriv [exp var]
  (cond (number? exp) 0
        (variable? exp) (if (same-variable? exp var) 1 0)
        (sum? exp) (make-sum-simplest (deriv (addend exp) var)
                                      (deriv (augend exp) var))
        (product? exp) (make-sum-simplest (make-product-simplest (multiplier exp)
                                                                 (deriv (multiplicand exp) var))
                                          (make-product-simplest (deriv (multiplier exp) var)
                                                                 (multiplicand exp)))
        :else (throw (Exception. (str "Unknown expression type: DERIV" exp)))))
