(ns chapter2.part4.operations-table
  (:require [chapter2.part4.complex-numbers-polar :as polar]
            [chapter2.part4.complex-numbers-rectangular :as rect]
            [chapter2.part4.common-stuff :as common]))

(def table (atom {}))

(defn put [operation type item]
  (swap! table assoc-in [operation type] item))

(defn get [operation type]
  (get-in @table [operation type]))

(defn install-rectangular-package []
  (do (put :real-part '(:rectangular) rect/real-part)
      (put :imag-part '(:rectangular) rect/imag-part)
      (put :magnitude '(:rectangular) rect/magnitude)
      (put :angle '(:rectangular) rect/angle)
      (put :make-from-real-imag :rectangular
           (fn [x y] (common/attach-tag :rectangular (rect/make-from-real-imag x y))))
      (put :make-from-mag-ang :rectangular
           (fn [x y] (common/attach-tag :rectangular (rect/make-from-mag-ang x y))))))

(defn install-polar-package []
  (do (put :real-part '(:polar) polar/real-part)
      (put :imag-part '(:polar) polar/imag-part)
      (put :magnitude '(:polar) polar/magnitude)
      (put :angle '(:polar) polar/angle)
      (put :make-from-real-imag :polar
           (fn [x y] (common/attach-tag :polar (polar/make-from-real-imag x y))))
      (put :make-from-mag-ang :polar
           (fn [x y] (common/attach-tag :polar (polar/make-from-mag-ang x y))))))

(install-rectangular-package)

(install-polar-package)
