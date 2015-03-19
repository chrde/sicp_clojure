(ns chapter2.part3.symbolic-differentiation-test
  (:require [clojure.test :refer :all]
            [chapter2.part3.symbolic-differentiation-exercises :refer :all]
            [chapter2.part3.symbolic-differentiation-samples :as ch3]))

(deftest equal?-test
  (testing "2.54 - equals?"
    (is (equal? '(this is a list) '(this is a list)))
    (is (not (equal? '(this is a list) '(this (is a) list))))))

(deftest deriv-exp-test
  (testing "2.56 - exponentiation selectors"
    (is (= 4 (base (make-exponentiation 4 5)))
        "base selector")
    (is (= 5 (exponent (make-exponentiation 4 5)))
        "exponent selector"))
  (testing "2.56 - simplify exponentiation"
    (is (= 1 (make-exponentiation-simplest 'x 0)))
    (is (= 'x (make-exponentiation-simplest 'x 1))))
  (testing "2.56 - deriv with exponentiation"
    (is (= '(* n (** x (+ n -1)))
           (deriv-exp '(** x n) 'x)))
    (is (= '(+ (* a (* 2 x)) b)
           (deriv-exp '(+ (+ (* a (** x 2)) (* b x)) c) 'x))
        "first deriv. of ax^2 + bx + c = 2ax + b")
    (is (= '(* a 2) (deriv-exp (deriv-exp '(+ (+ (* a (** x 2)) (* b x)) c) 'x) 'x))
        "second deriv. of ax^2 + bx +c = 2a")
    (is (= 0 (deriv-exp (deriv-exp (deriv-exp '(+ (+ (* a (** x 2)) (* b x)) c) 'x) 'x) 'x))
        "third deriv. of ax^2 + bx +c = 0")))

(deftest many-operations-test
  (testing "2.57 - selectors for expresions with more than 2 parameters"
    (is (= '(+ 2 3 4) (augend-many '(+ 1 2 3 4))) "augend")
    (is (= '(* 2 3 4) (multiplicand-many '(* 1 2 3 4))) "multiplicand"))
  (testing "2.57 - derive expressions with more than 2 parameters"
    (with-redefs [ch3/augend augend-many
                  ch3/multiplicand multiplicand-many]
      (is (= '(+ (* a (* 2 x)) b)
             (deriv-exp '(+ (* a (** x 2)) (* b x) c) 'x))))))

(deftest infix-notation-test
  (testing "2.58 - a) infix operators + *"
    (is (= 1 (deriv-exp-infix '(x + 3) 'x)))
    (is (= 3 (deriv-exp-infix '(x * 3) 'x)))
    (is (= '(a * (2 * x)) (deriv-exp-infix '(a * (** x 2)) 'x))))
  (testing "2.58 - b) standard alegraic notation"
    (is (= 12 (deriv-exp-infix '(3 * x + x + 2 * (x + 4 + y)) 'x)))
    (is (= '(2 * y) (deriv-exp-infix '(2 * x * y) 'x)))))

(run-tests)
