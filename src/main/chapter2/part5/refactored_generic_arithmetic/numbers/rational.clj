(ns chapter2.part5.refactored-generic-arithmetic.numbers.rational
  (:require [chapter2.part5.refactored-generic-arithmetic.common :as common]
            [chapter2.part5.refactored-generic-arithmetic.tables :as tables]
            [chapter1.samples :as chp1]
            [chapter2.part2.samples :refer [car cadr]]))

(defn- tag [x]
  (common/attach-tag :rational x))

(defn numer [x] (car x))

(defn denom [x] (cadr x))

(defn make-rat [n d]
  (let [g (chp1/gcd n d)]
    (list (/ n g) (/ d g))))

(defn add-rat [x y]
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defn sub-rat [x y]
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defn mul-rat [x y]
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(defn div-rat [x y]
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(defn zero?- [x]
  (zero? (numer x)))

(defn install-rational-package []
  (do
    (tables/put :add '(:rational :rational)
                (fn [x y] (tag (add-rat x y))))
    (tables/put :sub '(:rational :rational)
                (fn [x y] (tag (sub-rat x y))))
    (tables/put :mul '(:rational :rational)
                (fn [x y] (tag (mul-rat x y))))
    (tables/put :div '(:rational :rational)
                (fn [x y] (tag (div-rat x y))))
    (tables/put :make :rational
                (fn [n d] (tag (make-rat n d))))
    (tables/put :zero '(:rational)
                zero?-)))

(install-rational-package)

(zero?- '(0 1))

(defn make-rational [n d]
  ((tables/get :make :rational) n d))
