(ns chapter2.part5.generic-operations-test
  (:require [chapter2.part5.generic-operations :refer :all]
            [clojure.test :refer :all]))

(deftest attach-tag-test
  (testing "2.78 - attach-tag"
    (is (= 4 (attach-tag '+ 4)))
    (is (= '(+ + 4 3) (attach-tag '+ '(+ 4 3))))))

(deftest type-tag-test
  (testing "2.78 - type-tag"
    (is (= :number (type-tag (attach-tag '+ 4))))
    (is (= '+ (type-tag (attach-tag '+ '(+ 4 3)))))
    (is (thrown? Exception (type-tag false)))))

(deftest contents-test
  (testing "2.79 - contents-test"
    (is (= 4 (contents (attach-tag '+ 4))))
    (is (= '(+ 4 3) (contents (attach-tag '+ '(+ 4 3)))))
    (is (thrown? Exception (contents false)))))

(run-tests)
