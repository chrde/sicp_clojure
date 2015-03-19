(ns chapter2.part3.symbolic-differentiation-exercises
  (:require [chapter2.part2.samples :as ch2]
            [chapter2.part3.symbolic-differentiation-samples :refer :all]
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
        (sum? exp) (make-sum (deriv-exp (addend exp) var)
                             (deriv-exp (augend exp) var))
        (product? exp) (make-sum (make-product (multiplier exp)
                                               (deriv-exp (multiplicand exp) var))
                                 (make-product (deriv-exp (multiplier exp) var)
                                               (multiplicand exp)))
        (exponentiation? exp) (make-product (make-product (exponent exp)
                                                          (make-exponentiation-simplest
                                                            (base exp)
                                                            (make-sum (exponent exp) -1)))
                                            (deriv-exp (base exp) var))

        :else (throw (Exception. (str "Unknown expression type: DERIV-EXP" exp)))))

;; 2.57
(defn augend-many [s]
  (let [cddr (ch2/cdr (ch2/cdr s))]
    (if (nil? (ch2/cdr cddr))
      (ch2/car cddr)
      (cons '+ cddr))))

(defn multiplicand-many [s]
  (let [cddr (ch2/cdr (ch2/cdr s))]
    (if (nil? (ch2/cdr cddr))
      (ch2/car cddr)
      (cons '* cddr))))

;; 2.58
;;a)
(defn make-sum-infix [x y]
  (cond (=number? x 0) y
        (=number? y 0) x
        (and (number? x) (number? y)) (+ x y)
        :else (list x '+ y)))

(defn sum?-infix [x]
  (and (coll? x) (eq? (ch2/cadr x) '+)))

(defn addend-infix [x]
  (ch2/car x))

(defn make-product-infix [x y]
  (cond (or (=number? x 0) (=number? y 0)) 0
        (=number? x 1) y
        (=number? y 1) x
        (and (number? x) (number? y)) (* x y)
        :else (list x '* y)))

(defn product?-infix [x]
  (and (coll? x) (eq? (ch2/cadr x) '*)))

(defn multiplier-infix [s]
  (ch2/car s))
;;b)
(defn augend-infix [s]
  (let [cddr (ch2/cdr (ch2/cdr s))]
    (if (nil? (ch2/cdr cddr))
      (ch2/car cddr)
      cddr)))

(defn multiplicand-infix [s]
  (let [cddr (ch2/cdr (ch2/cdr s))]
    (if (nil? (ch2/cdr cddr))
      (ch2/car cddr)
      cddr)))

(defn deriv-exp-infix [exp var]
  (cond (number? exp) 0
        (variable? exp) (if (same-variable? exp var) 1 0)
        (sum?-infix exp) (make-sum-infix (deriv-exp-infix (addend-infix exp) var)
                                         (deriv-exp-infix (augend-infix exp) var))
        (product?-infix exp) (make-sum-infix (make-product-infix (multiplier-infix exp)
                                                                 (deriv-exp-infix (multiplicand-infix exp) var))
                                             (make-product-infix (deriv-exp-infix (multiplier-infix exp) var)
                                                                 (multiplicand-infix exp)))
        (exponentiation? exp) (make-product-infix (make-product-infix (exponent exp)
                                                                      (make-exponentiation-simplest
                                                                        (base exp)
                                                                        (make-sum-infix (exponent exp) -1)))
                                                  (deriv-exp-infix (base exp) var))

        :else (throw (Exception. (str "Unknown expression type: DERIV-EXP--INFIX" exp)))))
