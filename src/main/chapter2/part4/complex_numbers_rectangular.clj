(ns chapter2.part4.complex-numbers-rectangular
  (:require [chapter2.part4.common-stuff :refer :all]
            [chapter2.part2.samples :refer [car cadr]]
            [chapter1.samples :refer [sqr]]))

(defn real-part [z]
  (car z))

(defn imag-part [z]
  (cadr z))

(defn magnitude [z]
  (sqrt (+ (sqr (real-part z))
           (sqr (imag-part z)))))

(defn angle [z]
  (atan (imag-part z)
        (real-part z)))

(defn make-from-real-imag [x y]
  (list x y))

(defn make-from-mag-ang [r a]
  (list (* r (cos a)) (* r (sin a))))
