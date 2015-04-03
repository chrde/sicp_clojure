(ns chapter2.part5.generic-operations
  (:require [chapter2.part2.samples :refer [car cdr]]
            [chapter2.part4.common-stuff :as common]
            [chapter2.part4.complex-numbers-data-directed :as compl]
            [chapter2.part1.samples :as rat]
            [chapter2.part4.operations-table :as table]))

;; 2.78
(defn attach-tag [type-tag contents]
  (if (number? contents)
    contents
    (cons type-tag contents)))

(defn type-tag [datum]
  (cond (number? datum) :number
        (coll? datum) (car datum)
        :else (common/error "Bad tagged datum: TYPE-TAG" datum)))

(defn contents [datum]
  (cond (number? datum) datum
        (coll? datum) (cdr datum)
        :else (common/error ("Bad tagged datum: CONTENTS" datum))))
;; 2.79
(defn equ?-number [x y]
  (= x y))

(defn equ?-rational [x y]
  (= (* (rat/numer x) (rat/denom y)) (* (rat/numer y) (rat/denom x))))

(defn equ?-complex [x y]
  (and (= (compl/real-part x) (compl/real-part y)) (= (compl/imag-part x) (compl/imag-part y))))

(defn install-equ?-package []
  (do (table/put :equ '(:number :number) equ?-number)
      (table/put :equ '(:complex :complex) equ?-complex)
      (table/put :equ '(:rational :rational) equ?-rational)))

(defn equ? [x y]
  (with-redefs [common/attach-tag attach-tag
                common/type-tag type-tag
                common/contents contents]
    (compl/apply-generic :equ x y)))

(install-equ?-package)
;; 2.80
(defn zero?-number [x]
  (zero? x))

(defn zero?-rational [x]
  (zero? (rat/denom x)))

(defn zero?-complex [x]
  (= 0 (compl/imag-part x) (compl/imag-part x)))

(defn install-zero?-package []
  (do (table/put :zero '(:number) zero?-number)
      (table/put :zero '(:complex) zero?-complex)
      (table/put :zero '(:rational) zero?-rational)))

(defn zero?- [x]
  (with-redefs [common/attach-tag attach-tag
                common/type-tag type-tag
                common/contents contents]
    (compl/apply-generic :zero x)))

(install-zero?-package)

;; 2.81
