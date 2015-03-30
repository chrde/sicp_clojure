(ns chapter2.part3.representing-sets
  (:require [chapter2.part3.representing-sets-samples :as ch3]
            [chapter2.part2.samples :refer [car cdr cadr] :as ch2]
            [chapter2.part3.symbolic-differentiation :refer [equal?]]))

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

;; 2.62
(defn union-sorted-set [set1 set2]
  (cond (empty? set1) set2
        (empty? set2) set1
        :else (let [x1 (car set1)
                    x2 (car set2)]
                (cond
                  (= x1 x2) (cons x1 (union-sorted-set (cdr set1) (cdr set2)))
                  (< x1 x2) (cons x1 (union-sorted-set (cdr set1) set2))
                  :else (cons x2 (union-sorted-set set1 (cdr set2)))))))

;; 2.65
(defn union-tree-set [tree1 tree2]
  (ch3/list->tree (union-sorted-set (ch3/tree->list2 tree1)
                                    (ch3/tree->list2 tree2))))

(defn intersection-tree-set [tree1 tree2]
  (ch3/list->tree (ch3/intersection-sorted-set (ch3/tree->list2 tree1)
                                               (ch3/tree->list2 tree2))))

;; 2.66
(defn key- [record]
  (car record))

(defn value [record]
  (cadr record))

(defn make-record [a-key value]
  (list a-key value))

(defn lookup [given-key set-of-records]
  (cond (empty? set-of-records) false
        (equal? given-key (key- (ch3/entry set-of-records))) (value (ch3/entry set-of-records))
        (> given-key (key- (ch3/entry set-of-records))) (lookup given-key (ch3/right-branch set-of-records))
        :else (lookup given-key (ch3/left-branch set-of-records))))
