(ns chapter2.part5.refactored-generic-arithmetic.numbers.complex
  (:require [chapter2.part5.refactored-generic-arithmetic.common :as common]
            [chapter2.part5.refactored-generic-arithmetic.tables :as tables]
            [chapter1.samples :as chp1]
            [chapter2.part2.samples :refer [car cdr]]
            [chapter2.part4.complex-numbers-polar :as polar]
            [chapter2.part4.complex-numbers-rectangular :as rect]))

(defn- tag [x]
  (common/attach-tag :complex x))

(defn add-complex [z1 z2]
  (make-from-real-imag (+ (rect/real-part z1) (rect/real-part z2))
                       (+ (rect/imag-part z1) (rect/imag-part z2))))

(defn sub-complex [z1 z2]
  (make-from-real-imag (- (rect/real-part z1) (rect/real-part z2))
                       (- (rect/imag-part z1) (rect/imag-part z2))))

(defn mul-complex [z1 z2]
  (make-from-mag-ang (* (polar/magnitude z1) (polar/magnitude z2))
                     (+ (polar/angle z1) (polar/angle z2))))

(defn div-complex [z1 z2]
  (make-from-mag-ang (/ (polar/magnitude z1) (polar/magnitude z2))
                     (- (polar/angle z1) (polar/angle z2))))

(defn make-from-real-imag [x y]
  (rect/make-from-real-imag x y))

(defn make-from-mag-ang [r a]
  (polar/make-from-mag-ang))

(defn install-complex-package []
  (tables/put :add '(:complex :complex)
              (fn [z1 z2] (tag (add-complex z1 z2))))
  (tables/put :sub '(:complex :complex)
              (fn [z1 z2] (tag (sub-complex z1 z2))))
  (tables/put :mul '(:complex :complex)
              (fn [z1 z2] (tag (mul-complex z1 z2))))
  (tables/put :div '(:complex :complex)
              (fn [z1 z2] (tag (div-complex z1 z2))))
  (tables/put :make-from-real-imag :complex
              (fn [x y] (tag (make-from-real-imag x y))))
  (tables/put :make-from-mag-ang :complex
              (fn [r a] (tag (make-from-mag-ang r a)))))

(defn make-complex-from-real-imag [x y]
  ((get :make-from-real-imag :complex) x y))
(defn make-complex-from-mag-ang [r a]
  ((get :make-from-mag-ang :complex) r a))
