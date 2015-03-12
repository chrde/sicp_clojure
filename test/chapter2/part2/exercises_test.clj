(ns chapter2.part2.exercises-test
  (:require [clojure.test :refer :all]
            [chapter2.part2.exercises :refer :all]))

(deftest last-pair-test
  (testing "2.17 - last element of a list"
    (are [x y] (= x (last-pair y))
               4 (list 1 2 3 4)
               nil ()
               (list 1 2) (list 1 2 (list 1 2)))))

(deftest reverse-test
  (testing "2.18 - rever of a list"
    (are [x y] (= x (reverse- y))
               (list 3 2 1) '(1 2 3)
               (list 3 2 (list 1 2)) (list (list 1 2) 2 3)
               '() '())))

(deftest count-change-test
  (testing "2.19 estimated time to count change"
    (is (= (count-change 100 '(50 25 10 5 1))
           (count-change 100 '(1 5 10 25 50))
           292)))
  (testing "2.19 no-more?"
    (is (no-more? '()))
    (is (not (no-more? '(1)))))
  (testing "2.19 except-first-denomination"
    (are [x y] (= x (except-first-denomination y))
               nil '(1)
               '(2) '(1 2)))
  (testing "2.19 first-denomination"
    (are [x y] (= x (first-denomination y))
               nil '()
               1 '(1 2))))
