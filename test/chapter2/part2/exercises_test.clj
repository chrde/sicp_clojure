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
  (testing "2.23 for each"
    (let [input (range 4)
          x (atom (chapter2.part2.samples/length input))]
      (dorun (for-each (fn [_] (swap! x dec)) input))
      (is (zero? @x)))))

(deftest car-cdrs-in-tree-test
  (testing "2.25 - examples of car/cdr in trees"
    (is (= 7 (first-car-cdr (list 1 3 (list 5 7) 9))))
    (is (= 7 (second-car-cdr (list (list 7)))))
    (is (= 7 (third-car-cdr (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))))))

(deftest deep-reverse-test
  (testing "2.27 - deep reverse of trees"
    (is (= (list (list 4 3) (list 2 1))
           (deep-reverse (list (list 1 2) (list 3 4)))
           (deep-reverse- (list (list 1 2) (list 3 4)))))))

(deftest fringe-test
  (testing "2.28 - fringe (or flatten) a tree")
  (is (= '()
         (fringe '())))
  (is (= (range 10)
         (fringe (list 0 1 2 (list 3 4 (list 5 (list (list 6)))) 7 8 (list 9)))))
  (is (= '(1)
         (fringe (list (list (list (list 1))))))))

(deftest binary-mobile-test
  (let [branch1 (make-branch 2 4)
        branch2 (make-branch 4 2)
        mobile1 (make-mobile branch1 branch2)
        left (make-branch 0 mobile1)
        right (make-branch 1 2)
        mobile (make-mobile left right)
        mobile2 (make-mobile (make-branch 2 1) right)
        mobile3 (make-mobile (make-branch 3 mobile2) (make-branch 9 1))
        mobile4 (make-mobile (make-branch 8 2) (make-branch 4 mobile3))]
    (testing "2.29 - binary mobile - selectors"
      (is (= left (left-branch mobile))
          "2.29 - left branch selector")
      (is (= right (right-branch mobile))
          "2.29 - right branch selector")
      (is (= 1 (branch-length right))
          "2.29 - branch length")
      (is (= 2 (branch-structure right))
          "2.29 - branch structure "))
    (testing "2.29 - binary mobile - total weight"
      (is (= (+ (branch-structure branch1)
                (branch-structure branch2))
             (total-weight mobile1))
          "2.29 - weight of mobile without nested branches")
      (is (= (+ (branch-structure branch1)
                (branch-structure branch2)
                (branch-structure right))
             (total-weight mobile))
          "2.29 - weight of a complex mobile")
      (testing "2.29 - balanced mobile"
        (is (true? (balanced-structure? mobile1))
            "2.29 - simple balanced mobile")
        (is (false? (balanced-structure? (make-mobile branch1 right))))
        (is (false? (balanced-structure? (make-mobile branch1 (make-branch 2 3))))
            "2.29 - simple unbalanced mobile because of weight")
        (is (false? (balanced-structure? (make-mobile branch1 (make-branch 3 4))))
            "2.29 - simple unbalanced mobile because of length")
        (is (true? (balanced-structure? mobile4)) "2.29 - nested balanced mobile")
        ))))

(deftest square-tree-test
  (let [input (list 1 (list 2 (list 3 4) 5) (list 6 7))
        output (list 1 (list 4 (list 9 16) 25) (list 36 49))]
    (testing "2.30 apply square to a tree"
      (is (= output (square-tree input))))))

(deftest map-tree-test
  (let [input (list 1 (list 2 (list 3 4) 5) (list 6 7))
        output (list 1 (list 4 (list 9 16) 25) (list 36 49))]
    (testing "2.31 mapping over a tree"
      (is (= output (tree-map chapter1.samples/sqr input)))
      (is (= output (tree-map1 chapter1.samples/sqr input))))))

(run-tests)
