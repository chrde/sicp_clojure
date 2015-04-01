(ns chapter2.part4.complex-numbers-direct-invocation
  (:require [chapter2.part2.samples :refer [car cdr]]
            [chapter1.samples :refer [sqr]]
            [chapter2.part4.common-stuff :refer :all]
            [chapter2.part4.complex-numbers-rectangular :as rect]
            [chapter2.part4.complex-numbers-polar :as polar]))

;;; Rectangular representation
(defn rectangular? [z]
  (eq? (type-tag z) :rectangular))

(defn make-from-real-imag [x y]
  (attach-tag :rectangular rect/make-from-real-imag x y))

;;; Polar representation
(defn polar? [z]
  (eq? (type-tag z) :polar))

(defn make-from-mag-ang [r a]
  (attach-tag :polar polar/make-from-mag-ang r a))

;; Generic selectors
(defn real-part [z]
  (cond (rectangular? z) (rect/real-part (contents z))
        (polar? z) (polar/real-part (contents z))
        :else (error "Unknown type: REAL-PART" z)))

(defn imag-part [z]
  (cond (rectangular? z) (rect/imag-part (contents z))
        (polar? z) (polar/imag-part (contents z))
        :else (error "Unknown type: IMAG-PART" z)))

(defn magnitude [z]
  (cond (rectangular? z) (rect/magnitude (contents z))
        (polar? z) (polar/magnitude (contents z))
        :else (error "Unknown type: MAGNITUDE" z)))

(defn angle [z]
  (cond (rectangular? z) (rect/angle (contents z))
        (polar? z) (polar/angle (contents z))
        :else (error "Unknown type: ANGLE" z)))

;; Operations with complex numbers
(defn add-complex [z1 z2]
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(defn sub-complex [z1 z2]
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

(defn mul-complex [z1 z2]
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

(defn div-complex [z1 z2]
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))
