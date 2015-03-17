(ns chapter2.part2.exercises-test
  (:require [clojure.test :refer :all]
            [chapter2.part2.exercises :refer :all]
            [chapter2.part2.samples :as ch2]))

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
          "left branch selector")
      (is (= right (right-branch mobile))
          "right branch selector")
      (is (= 1 (branch-length right))
          "branch length")
      (is (= 2 (branch-structure right))
          "branch structure "))
    (testing "2.29 - binary mobile - total weight"
      (is (= (+ (branch-structure branch1)
                (branch-structure branch2))
             (total-weight mobile1))
          "weight of mobile without nested branches")
      (is (= (+ (branch-structure branch1)
                (branch-structure branch2)
                (branch-structure right))
             (total-weight mobile))
          "weight of a complex mobile")
      (testing "2.29 - balanced mobile"
        (is (true? (balanced-structure? mobile1))
            "simple balanced mobile")
        (is (false? (balanced-structure? (make-mobile branch1 right))))
        (is (false? (balanced-structure? (make-mobile branch1 (make-branch 2 3))))
            "simple unbalanced mobile because of weight")
        (is (false? (balanced-structure? (make-mobile branch1 (make-branch 3 4))))
            "simple unbalanced mobile because of length")
        (is (true? (balanced-structure? mobile4)) "2.29 - nested balanced mobile")
        ))))

(deftest square-tree-test
  (let [input (list 1 (list 2 (list 3 4) 5) (list 6 7))
        output (list 1 (list 4 (list 9 16) 25) (list 36 49))]
    (testing "2.30 - apply square to a tree"
      (is (= output (square-tree input))))))

(deftest map-tree-test
  (let [input (list 1 (list 2 (list 3 4) 5) (list 6 7))
        output (list 1 (list 4 (list 9 16) 25) (list 36 49))]
    (testing "2.31 - mapping over a tree"
      (is (= output (tree-map chapter1.samples/sqr input)))
      (is (= output (tree-map1 chapter1.samples/sqr input))))))

(deftest subsets-test
  (testing "2.32 - subsets"
    (is (= (list '()) (subsets nil)))
    (is (= (list '() '(0)) (subsets '(0))))
    (is (= (set (list '() '(0) '(1) '(0 1)))
           (set (subsets '(0 1)))))))

(deftest accumulate-test
  (testing "2.33 - accumulate as the basic building block"
    (is (= '(1 4 9 16) (map-acc chapter1.samples/sqr (list 1 2 3 4)))
        "map based on accumulate")
    (is (= 4 (length-acc (list 1 2 3 4)))
        "length based on accumulate")
    (is (= '(1 2 3 4) (append-acc (list 1 2) (list 3 4)))
        "append based on accumulate")))

(deftest horner-eval-test
  (testing "2.34 - Horner's rule"
    (is (= 79 (horner-eval 2 (list 1 3 0 5 0 1))))))

(deftest count-leaves-acc-test
  (testing "2.35 - count leaves based on accumulate"
    (is (= 4 (count-leaves-acc (list (list 1 (list (list (list 2)) (list 3 4)))))))))

(deftest accumulate-n-test
  (testing "2.36 - accumulate with many sequences"
    (is (= '(22 26 30)
           (accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))))))

(deftest matrix-operations-test
  (testing "2.37 - matrix operations"
    (is (= 32 (dot-product (list 1 2 3) (list 4 5 6)))
        "dot product")
    (is (= '(14 22) (matrix-*-vector (list (list 3 4) (list 5 6)) (list 2 2)))
        "matrix * vector")
    (is (= (list (list 1 3 5) (list 2 4 6)) (transpose (list (list 1 2) (list 3 4) (list 5 6))))
        "transpose a matrix")
    (is (= (list (list 20 -22 22) (list -32 1 -1))
           (matrix-*-matrix (list (list 6 4) (list -2 5))
                            (list (list 6 -3 3) (list -4 -1 1))))
        "matrix * matrix")))

(deftest folds-comparison-test
  (testing "2.38 - fold right vs fold left"
    (is (= (/ 3 2)
           (fold-right / 1 (list 1 2 3))))
    (is (= (/ 1 6)
           (fold-left / 1 (list 1 2 3))))
    (is (= (list 1 (list 2 (list 3 nil)))
           (fold-right list nil (list 1 2 3))))
    (is (= (list (list (list nil 1) 2) 3)
           (fold-left list nil (list 1 2 3))))))

(deftest reverse-fold-test
  (testing "2.39 - reverse based on fold"
    (is (= '(4 3 2 1) (reverse-fold-left '(1 2 3 4)))
        "reverse based on fold-left")
    (is (= '(4 3 2 1) (reverse-fold-right '(1 2 3 4)))
        "reverse based on fold-right")))

(deftest unique-pairs-test
  (testing "2.40 - generating unique pairs"
    (is (nil? (unique-pairs 0)))
    (is (nil? (unique-pairs 1)))
    (is (= (list (list 2 1)) (unique-pairs 2)))
    (is (= (set (list (list 2 1) (list 3 1) (list 3 2)))
           (set (unique-pairs 3))))))

(deftest ordered-triples-test
  (testing "2.41 - find all ordered triples"
    (is (= '() (find-triples 0)))
    (is (= '() (find-triples 1)))
    (is (= '() (find-triples 2)))
    (is (= '() (find-triples 5)))
    (is (= (set (list (list 6 2 1) (list 5 3 1) (list 4 3 2)))
           (set (find-triples 9)))))
  (testing "2.41 - find all ordered triples - flatmap version"
    (is (nil? (find-triples-flatmap 0)))
    (is (nil? (find-triples-flatmap 1)))
    (is (nil? (find-triples-flatmap 2)))
    (is (nil? (find-triples-flatmap 5)))
    (is (= (set (list (list 6 2 1) (list 5 3 1) (list 4 3 2)))
           (set (find-triples-flatmap 9))))))

(deftest n-queens-test
  (testing "2.42 - collides? to place a queen"
    (is (false? (collides? (place-queen 1 1) (place-queen 3 1)))
        "collides? doesn't check for the same column")
    (is (true? (collides? (place-queen 1 1) (place-queen 1 2)))
        "collides? for the same row")
    (is (true? (collides? (place-queen 3 2) (place-queen 5 4)))
        "collides? for the diagonal down-right")
    (is (true? (collides? (place-queen 3 2) (place-queen 2 1)))
        "collides? for the diagonal up-left")
    (is (true? (collides? (place-queen 3 2) (place-queen 4 1)))
        "collides? for the diagonal down-left")
    (is (true? (collides? (place-queen 3 2) (place-queen 3 4)))
        "collides? for the diagonal up-right"))
  (testing "2.42 - safe? to place a queen"
    (is (safe? 1 (list)))
    (is (not (safe? 2 (list (place-queen 1 1) (place-queen 2 2)))))
    (is (safe? 3 (list (place-queen 1 2) (place-queen 3 3))))
    (is (safe? 8 (list (place-queen 4 1) (place-queen 6 2) (place-queen 8 3) (place-queen 5 8)
                       (place-queen 7 5) (place-queen 2 4) (place-queen 1 6) (place-queen 3 7)))))
  (testing "2.42 - n queens problem"
    (are [x y] (= x (ch2/length (queens y)))
               1 1
               0 2
               0 3
               2 4
               92 8)
    ))
(run-tests)
