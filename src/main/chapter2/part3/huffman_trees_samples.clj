(ns chapter2.part3.huffman-trees-samples
  (:require [chapter2.part2.samples :refer [car cdr cadr caddr cadddr] :as ch2]))

(def eq? =)

(defn make-leaf [symbol weight]
  (list 'leaf symbol weight))

(defn leaf? [object]
  (eq? (car object) 'leaf))

(defn symbol-leaf [x]
  (cadr x))

(defn weight-leaf [x]
  (caddr x))

(defn left-branch [tree]
  (car tree))

(defn right-branch [tree]
  (cadr tree))

(defn symbols [tree]
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)))

(defn weight [tree]
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)))

(defn make-code-tree [left right]
  (list left
        right
        (ch2/append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(defn choose-branch [bit branch]
  (cond (= bit 0) (left-branch branch)
        (= bit 1) (right-branch branch)
        :else (throw (Exception. (str "bad bit: CHOOSE BRANCH" bit)))))

(defn decode
  ([bits tree] (decode bits tree tree))
  ([bits tree current-branch]
   (if (nil? bits)
     '()
     (let [next-branch (choose-branch (car bits) current-branch)]
       (if (leaf? next-branch)
         (cons (symbol-leaf next-branch)
               (decode (cdr bits) tree tree))
         (decode (cdr bits) tree next-branch))))))

(defn adjoin-huffman-set [x set]
  (cond (empty? set) (list x)
        (< (weight x) (weight (car set))) (cons x set)
        :else (cons (car set) (adjoin-huffman-set x (cdr set)))))

(defn make-leaf-set [pairs]
  (if (nil? pairs)
    '()
    (let [pair (car pairs)]
      (adjoin-huffman-set (make-leaf (car pair)
                                     (cadr pair))
                          (make-leaf-set (cdr pairs))))))
