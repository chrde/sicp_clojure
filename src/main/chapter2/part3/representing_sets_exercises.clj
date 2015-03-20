(ns chapter2.part3.representing-sets-exercises
  (:require [chapter2.part3.representing-sets-samples :as ch3]
            [chapter2.part2.samples :as ch2]))

;; 2.59
(defn union-set [set1 set2]
  (if (nil? set2)
    set1
    (union-set (ch3/adjoin-set (ch2/car set2) set1)
               (ch2/cdr set2))))

;; 2.60
(defn element-of-set?-dup [x set]
  (ch3/element-of-set? x set))

(defn adjoin-set-dup [x set]
  (cons x set))

(defn intersection-set-dup [set1 set2]
  (cond (or (nil? set1) (nil? set2)) '()
        (element-of-set?-dup (ch2/car set1) set2) (cons (ch2/car set1)
                                                        (intersection-set-dup (ch2/cdr set1) set2))
        :else (intersection-set-dup (ch2/cdr set1) set2)))

(defn union-set-dup [set1 set2]
  (if (nil? set2)
    set1
    (union-set-dup (adjoin-set-dup (ch2/car set2) set1)
                   (ch2/cdr set2))))
