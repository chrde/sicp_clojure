(ns chapter3.part1.assignment-and-state-test
  (:require [clojure.test :refer :all]
            [chapter3.part1.assignment-and-state :refer :all]
            [chapter3.part1.assignment-and-state-samples :as samples]))

(deftest make-accumulator-test
  (testing "3.1 - Making simple accumulator"
    (let [A (make-accumulator 5)]
      (is (= 15 (A 10)))
      (is (= 19 (A 4))))))

(deftest make-monitored-test
  (testing "3.2 - Count number of invocations of a function"
    (let [s (make-monitored inc)]
      (is (= 2 (s 1)))
      (is (= 1 (s :how-many-calls)))
      (is (= 0 (do (s 1)
                   (s :reset-count)
                   (s :how-many-calls)))))))

(deftest make-acount-password-test
  (testing "3.3 - Make an account with an initial password"
    (let [A (make-account-password 100 :password)]
      (is (= 60 ((A :password :withdraw) 40)))
      (is (thrown-with-msg? Exception #"Wrong password" ((A :password2 :withdraw) 10))))))
