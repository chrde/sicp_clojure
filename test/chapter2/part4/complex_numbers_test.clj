(ns chapter2.part4.complex-numbers-test
  (:require [clojure.test :refer :all]
            [chapter2.part4.complex-numbers :refer :all]))

(deftest deriv-data-test
  (testing "2.73 - deriv with data data-directed-style"
    (is (= '(+ 1) (deriv '(+ + 3 x) 'x)))
    (is (= '(* 4) (deriv '(* * 4 x) 'x)))
    (is (= '(** 1) (deriv '(** ** x 1) 'x)))))
