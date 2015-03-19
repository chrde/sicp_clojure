(ns chapter2.part3.exercises
  (:require [chapter2.part2.samples :as ch2]))

;; 2.54
(defn equal? [x y]
  (if (and (coll? x) (coll? y))
    (and (equal? (ch2/car x) (ch2/car y))
         (equal? (ch2/cdr x) (ch2/cdr y)))
    (= x y)))
