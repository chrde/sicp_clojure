(ns chapter2.part4.complex-numbers-polar
  (:require [chapter2.part4.common-stuff :refer :all]
            [chapter2.part2.samples :refer [car cdr]]
            [chapter1.samples :refer [sqr]]))

(defn magnitude [z]
  (car z))

(defn angle [z]
  (cdr z))

(defn real-part [z]
  (* (magnitude z) (cos (angle z))))

(defn imag-part [z]
  (* (magnitude z) (sin (angle z))))

(defn make-from-real-imag [x y]
  (attach-tag 'polar
              (cons (sqrt (+ (sqr x) (sqr y)))
                    (atan y x))))

(defn make-from-mag-ang [r a]
  (attach-tag 'polar (cons r a)))
