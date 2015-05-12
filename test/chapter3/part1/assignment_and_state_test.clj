(ns chapter3.part1.assignment-and-state-test
  (:require [clojure.test :refer :all]
            [chapter3.part1.assignment-and-state :refer :all]))

(deftest make-accumulator-test
  (testing "3.1 - Making simple accumulator"
    (let [A (make-accumulator 5)]
      (is (= 15 (A 10)))
      (is (= 19 (A 4))))))
