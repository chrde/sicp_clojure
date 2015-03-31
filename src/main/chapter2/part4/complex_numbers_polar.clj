(ns chapter2.part4.complex-numbers-polar
  (:require [chapter2.part4.common-stuff :refer :all]
            [chapter2.part2.samples :refer [car cdr]]
            [chapter1.samples :refer [sqr]]))

(defn magnitude-polar [z]
  (car z))

(defn angle-polar [z]
  (cdr z))

(defn make-from-mag-ang-polar [r a]
  (attach-tag 'polar (cons r a)))

(defn make-from-real-imag-polar [x y]
  (attach-tag 'polar
              (cons (sqrt (+ (sqr x) (sqr y)))
                    (atan y x))))

(defn real-part-polar [z]
  (* (magnitude-polar z) (cos (angle-polar z))))

(defn imag-part-polar [z]
  (* (magnitude-polar z) (sin (angle-polar z))))
