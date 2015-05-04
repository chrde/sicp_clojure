(ns chapter2.part5.refactored-generic-arithmetic.numbers.scheme
  (:require [chapter2.part5.refactored-generic-arithmetic.common :as common]
            [chapter2.part5.refactored-generic-arithmetic.tables :as tables]))

(defn- tag [x]
  (common/attach-tag :scheme-number x))

(defn install-scheme-number-package []
  (do
    (tables/put :add '(:scheme-number :scheme-number)
                (fn [x y] (tag (+ x y))))
    (tables/put :sub '(:scheme-number :scheme-number)
                (fn [x y] (tag (- x y))))
    (tables/put :mul '(:scheme-number :scheme-number)
                (fn [x y] (tag (* x y))))
    (tables/put :div '(:scheme-number :scheme-number)
                (fn [x y] (tag (/ x y))))
    (tables/put :zero '(:scheme-number)
                (fn [x] (zero? x)))
    (tables/put :equ '(:scheme-number :scheme-number)
                (fn [x y] (= x y)))
    (tables/put :make :scheme-number (fn [x] (tag x)))))

(defn make-scheme-number [n]
  ((tables/get :make :scheme-number) n))

(install-scheme-number-package)
