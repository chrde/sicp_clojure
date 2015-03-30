(ns chapter2.part3.representing-sets-test
  (:require [clojure.test :refer :all]
            [chapter2.part3.representing-sets :refer :all]
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

(deftest adjoin-sorted-set-test
  (testing "2.61 - adding elements to a sorted list"
    (is (= (list 4) (adjoin-sorted-set 4 '())))
    (is (= (list 1 2 3) (adjoin-sorted-set 2 '(1 3))))))

(deftest union-sorted-set-test
  (testing "2.62 - union of sorted sets"
    (is (= '(2 4) (union-sorted-set '() '(2 4))))
    (is (= '(2 4) (union-sorted-set '(2 4) '())))
    (is (= '(1 2 3 4) (union-sorted-set '(1 3) '(2 4))))))

(deftest union-tree-set-test
  (testing "2.65 - union of sorted trees"
    (is (= '(4
             (2 (1 () ()) (3 () ()))
             (6 (5 () ()) (7 () ())))
           (union-tree-set (ch3/list->tree (list  1 2 3 4)) (ch3/list->tree (list 5 6 7)))))))

(deftest intersection-tree-set-test
  (testing "2.65 - intersection of sorted trees"
    (is (= '(4 () ())
           (intersection-tree-set (ch3/list->tree (list 1 2 3 4))
                                  (ch3/list->tree (list 4 5 6 7)))))))

(deftest lookup-test
  (testing "2.66 - lookup in a keyed tree"
    (let [records (ch3/make-tree (make-record 2 :value2)
                                 (ch3/make-tree  (make-record 1 :value1) '() '())
                                 (ch3/make-tree  (make-record 3 :value3) '() '()))]
      (is (= :value1 (lookup 1 records)))
      (is (= :value2 (lookup 2 records)))
      (is (not (lookup 4 records))))))
