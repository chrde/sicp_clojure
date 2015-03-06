(ns chapter2.part1.samples
  (:require [chapter1.samples :as ch1]))

(defn make-rat [n d]
  (let [g (ch1/gcd n d)]
    (list (quot n g) (quot d g))))

(defn numer [x]
  (first x))

(defn denom [x]
  (second x))

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

(defn equal-rat [x y]
  (= (* (numer x) (denom y))
     (* (denom x) (numer y))))

(defn print-rat [x]
  (println (str (numer x) "/" (denom x))))
