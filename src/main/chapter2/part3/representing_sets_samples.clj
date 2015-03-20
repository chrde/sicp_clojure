(ns chapter2.part3.representing-sets-samples
  (:require [chapter2.part2.samples :as ch2]))

(defn element-of-set? [x set]
  (cond (nil? set) false
        (= x (ch2/car set)) true
        :else (element-of-set? x (ch2/cdr set))))

(defn adjoin-set [x set]
  (if (element-of-set? x set)
    set
    (cons x set)))

(defn intersection-set [set1 set2]
  (cond (or (nil? set1) (nil? set2)) '()
        (element-of-set? (ch2/car set1) set2) (cons (ch2/car set1)
                                                    (intersection-set (ch2/cdr set1) set2))
        :else (intersection-set (ch2/cdr set1) set2)))

