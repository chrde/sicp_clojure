(ns chapter2.part3.representing-sets-samples
  (:require [chapter2.part2.samples :refer [car cdr cadr caddr] :as ch2]))

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

(defn entry [tree]
  (car tree))

(defn left-branch [tree]
  (cadr tree))

(defn right-branch [tree]
  (caddr tree))

(defn make-tree [entry left right]
  (list entry left right))

(defn element-of-tree-set? [x set]
  (cond (nil? set) false
        (= x (entry set)) true
        (< x (entry set) (element-of-tree-set? x (left-branch set)))
        (> x (entry set) (element-of-tree-set? (right-branch set)))))

(defn adjoin-tree-set [x set]
  (cond (nil? set) (make-tree x)
        (= x (entry set)) set
        (< x (entry set)) (make-tree (entry set)
                                     (adjoin-tree-set x (left-branch set))
                                     (right-branch set))
        (> x (entry set)) (make-tree (entry set)
                                     (left-branch set)
                                     (adjoin-tree-set x (right-branch set)))))

(defn tree->list2
  ([tree] (tree->list2 tree '()))
  ([tree result-list]
   (if (empty? tree)
     result-list
     (tree->list2 (left-branch tree)
                  (cons (entry tree)
                        (tree->list2 (right-branch tree)
                                     result-list))))))

(defn partial-tree [elts n]
  (if (zero? n)
    (cons '() elts)
    (let [left-size (quot (dec n) 2)
          left-result (partial-tree elts left-size)
          left-tree (car left-result)
          non-left-elts (cdr left-result)
          right-size (- n (inc left-size))
          this-entry (car non-left-elts)
          right-result (partial-tree (cdr non-left-elts) right-size)
          right-tree (car right-result)
          remaining-elts (cdr right-result)]
      (cons (make-tree this-entry
                       left-tree
                       right-tree)
            remaining-elts))))

(defn list->tree [elements]
  (car (partial-tree elements (ch2/length elements))))
