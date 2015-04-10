(ns chapter2.part5.generic-operations-test
  (:require [chapter2.part5.generic-operations :refer :all]
            [clojure.test :refer :all]
            [chapter2.part4.common-stuff :as common]
            [chapter2.part4.complex-numbers-data-directed :as p4]))

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
  (testing "2.78 - contents-test"
    (is (= 4 (contents (attach-tag '+ 4))))
    (is (= '(+ 4 3) (contents (attach-tag '+ '(+ 4 3)))))
    (is (thrown? Exception (contents false)))))

(deftest equ?-test
  (testing "2.79 - equ? of numbers"
    (is (equ? 5 5))))

(deftest zero?-test
  (testing "2.80 - zero? of numbers"
    (is (zero?- 0))))

(deftest apply-coercion-test
  (testing "2.81 - apply method after coercion"
    (with-redefs [common/attach-tag attach-tag
                  common/type-tag type-tag
                  common/contents contents]
      (is (thrown-with-msg? Exception #"no coercion" (apply-generic-coercion- :equ '(:tag :a) '(:tag :a))))
      (is (apply-generic-coercion- :equ 4 (- 6 2))))))

(deftest apply-generic-smart-coercion-test
  (testing "2.82 - find coercions from first type"
    (is (empty? (find-coercions-from-type :number '(:character :string))))
    (is (= 2 (count (find-coercions-from-type :number '(:number :number))))))
  (testing "2.82 - find common coercion"
    (is (nil? (find-common-coercion '(:number :character :strings))))
    (is (= (repeat 3 identity)  (find-common-coercion '(:number :number :number)))))
  (testing "2.82 - apply generic with smart coercions"
    (with-redefs [common/attach-tag attach-tag
                  common/type-tag type-tag
                  common/contents contents]
      (is (apply-generic-smart-coercion :equ '(4 (:super-number 4))))
      (is (not (apply-generic-smart-coercion :equ '(4 (:super-number 5)))))
      (is (thrown-with-msg? Exception #"Automatic coercion"
                            (apply-generic-smart-coercion :equ '(4 (:other-number 5))))))))

(deftest raise-numbers-test
  (testing "2.83 - raise numbers"
    (is (= (integer->rational 3) (raise (common/attach-tag :integer 3))))))


(deftest auto-coerce-numbers-test
  (testing "2.84 - is subtype"
    (is (is-subtype? :integer :real)))
  (testing "2.84 - coerce numbers"
    (is (= '(:rational (5 1)) (coerce-to '(:integer 5) '(:rational (4 1)))))
    ;(is (= '(5 1) (coerce-to '(:rational (4 1)) '(:integer 5))))
   ; (is (= 4.0 (coerce-to (':integer 4) '(:real 3.2))))
    ))

(run-tests)
