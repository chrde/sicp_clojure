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
  (testing "2.19 - estimated time to count change"
    (is (= (count-change 100 '(50 25 10 5 1))
           (count-change 100 '(1 5 10 25 50))
           292)))
  (testing "2.19 - no-more?"
    (is (no-more? '()))
    (is (not (no-more? '(1)))))
  (testing "2.19 - except-first-denomination"
    (are [x y] (= x (except-first-denomination y))
               nil '(1)
               '(2) '(1 2)))
  (testing "2.19 - first-denomination"
    (are [x y] (= x (first-denomination y))
               nil '()
               1 '(1 2))))

(deftest same-parity-test
  (testing "2.20 - parity of x"
    (is (= even? (parity 2)))
    (is (= odd? (parity 1)))
    (is (false? ((parity 0) 1)) "0 has no parity")
    (is (false? ((parity 0) 2)) "0 has no parity"))
  (testing "2.20 - same parity as the first argument"
    (are [x y] (= x (apply same-parity y))
               '() nil
               '() '()
               '() '(0)
               '(2 4 6) '(2 3 4 5 6 7)
               '(1 3 5 7) '(1 2 3 4 5 6 7))))

(deftest square-list-test
  (testing "2.21 - square of a list with different implementations"
    (is (= (square-list-recur '(1 2 3 4))
           '(1 4 9 16)))
    (is (= (square-list '(1 2 3 4))
           '(1 4 9 16)))))


(deftest for-each-test
  (testing "2.22 for each"
    (let [input (range 4)
          x (atom (chapter2.part2.samples/length input))]
      (dorun (for-each (fn [_] (swap! x dec)) input))
      (is (zero? @x)))))

(run-tests)
