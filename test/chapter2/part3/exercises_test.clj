(ns chapter2.part3.exercises-test
  (:require [clojure.test :refer :all]
            [chapter2.part3.exercises :refer :all]
            [chapter2.part3.samples :as ch3]))

(deftest equal?-test
  (testing "2.54 - equals?"
    (is (equal? '(this is a list) '(this is a list)))
    (is (not (equal? '(this is a list) '(this (is a) list))))))

(run-tests)
