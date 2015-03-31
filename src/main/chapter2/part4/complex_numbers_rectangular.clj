(ns chapter2.part4.complex-numbers-rectangular
  (:require [chapter2.part4.common-stuff :refer :all]
            [chapter2.part2.samples :refer [car cdr]]
            [chapter1.samples :refer [sqr]]))

(defn real-part-rectangular [z]
  (car z))

(defn imag-part-rectangular [z]
  (cdr z))

(defn make-from-real-imag-rectangular [x y]
  (attach-tag 'rectangular (cons x y)))

(defn make-from-mag-ang-rectangular [r a]
  (attach-tag 'rectangular
              (cons (* r (cos a)) (* r (sin a)))))

(defn magnitude-rectangular [z]
  (sqrt (+ (sqr (real-part-rectangular z))
           (sqr (imag-part-rectangular z)))))


(defn angle-rectangular [z]
  (atan (imag-part-rectangular z)
        (real-part-rectangular z)))
