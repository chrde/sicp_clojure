(ns chapter2.part4.complex-numbers-rectangular
  (:require [chapter2.part4.common-stuff :refer :all]
            [chapter2.part2.samples :refer [car cdr]]
            [chapter1.samples :refer [sqr]]))

(defn real-part [z]
  (car z))

(defn imag-part [z]
  (cdr z))

(defn magnitude [z]
  (sqrt (+ (sqr (real-part z))
           (sqr (imag-part z)))))

(defn angle [z]
  (atan (imag-part z)
        (real-part z)))

(defn make-from-real-imag [x y]
  (attach-tag 'rectangular (cons x y)))

(defn make-from-mag-ang [r a]
  (attach-tag 'rectangular
              (cons (* r (cos a)) (* r (sin a)))))
