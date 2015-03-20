(ns chapter2.part3.representing-sets-exercises
  (:require [chapter2.part3.representing-sets-samples :as ch3]
            [chapter2.part2.samples :refer [car cdr] :as ch2]))

;; 2.59
(defn union-set [set1 set2]
  (if (nil? set2)
    set1
    (union-set (ch3/adjoin-set (car set2) set1)
               (cdr set2))))

;; 2.60
(defn element-of-set?-dup [x set]
  (ch3/element-of-set? x set))

(defn adjoin-set-dup [x set]
  (cons x set))

(defn intersection-set-dup [set1 set2]
  (cond (or (nil? set1) (nil? set2)) '()
        (element-of-set?-dup (car set1) set2) (cons (car set1)
                                                        (intersection-set-dup (cdr set1) set2))
        :else (intersection-set-dup (cdr set1) set2)))

(defn union-set-dup [set1 set2]
  (if (nil? set2)
    set1
    (union-set-dup (adjoin-set-dup (car set2) set1)
                   (cdr set2))))

;; 2.61
(defn adjoin-sorted-set [x set]
  (cond (empty? set) (list x)
        (< x (car set)) (cons x set)
        (= (car set) x) set
        :else (cons (car set) (adjoin-sorted-set x (cdr set)))))
