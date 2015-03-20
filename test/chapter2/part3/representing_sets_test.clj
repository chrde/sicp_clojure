(ns chapter2.part3.representing-sets-test
  (:require [clojure.test :refer :all]
            [chapter2.part3.representing-sets-exercises :refer :all]
            [chapter2.part3.representing-sets-samples :as ch3]))

(deftest union-set-test
  (testing "2.59 - union of sets"
    (is (not (ch3/element-of-set? 4 (union-set (list 3) (list 2)))))
    (is (ch3/element-of-set? 4 (union-set (list) (list 4))))))

(deftest set-with-duplicates-test
  (testing "2.60 - allowing duplicates in sets"
    (is (element-of-set?-dup 4 (list 4 4))
        "element-of-set?-dup")
    (is (= (list 4 4) (adjoin-set-dup 4 (list 4)))
        "adjoin-set-dup")
    (is (= (list 2 2 3) (union-set-dup (list 3) (list 2 2)))
        "union-set-dup")
    (is (= (list) (intersection-set-dup (list 3) (list 2 2)))
        "intersection-set-dup")))

(run-tests)
