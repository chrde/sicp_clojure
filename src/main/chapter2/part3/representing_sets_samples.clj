(ns chapter2.part3.representing-sets-samples
  (:require [chapter2.part2.samples :refer [car cdr] :as ch2]))

(defn element-of-set? [x set]
  (cond (nil? set) false
        (= x (car set)) true
        :else (element-of-set? x (cdr set))))

(defn adjoin-set [x set]
  (if (element-of-set? x set)
    set
    (cons x set)))

(defn intersection-set [set1 set2]
  (cond (or (nil? set1) (nil? set2)) '()
        (element-of-set? (car set1) set2) (cons (car set1)
                                                    (intersection-set (cdr set1) set2))
        :else (intersection-set (cdr set1) set2)))

(defn element-of-sorted-set? [x set]
  (cond (nil? set) false
        (= x (car set)) true
        (< x (car set)) false
        :else (element-of-sorted-set? x (cdr set))))

(defn intersection-sorted-set [set1 set2]
  (if (or (nil? set1) (nil? set2))
    '()
    (let [x1 (car set1)
          x2 (car set2)]
      (cond (= x1 x2) (cons x1 (intersection-sorted-set (cdr set1) (cdr set2)))
            (< x1 x2) (intersection-sorted-set (cdr set1) set2)
            :else (intersection-sorted-set set1 (cdr set2))))))
