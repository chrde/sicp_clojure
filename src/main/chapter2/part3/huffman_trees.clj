(ns chapter2.part3.huffman-trees
  (:require [chapter2.part3.huffman-trees-samples :refer :all]
            [chapter2.part2.samples :refer [car cdr cddr cadr caddr append] :as ch2]
            [chapter2.part3.representing-sets-samples :refer [element-of-set?]]))

;; 2.67
(def sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(def sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree (make-leaf 'B 2)
                                  (make-code-tree (make-leaf 'D 1)
                                                  (make-leaf 'C 1)))))

;; 2.68
(defn contains-symbol? [symbol branch]
  (if (leaf? branch)
    (eq? symbol (symbol-leaf branch))
    (element-of-set? symbol (symbols branch))))

(defn encode-symbol [symbol tree]
  (if (leaf? tree)
    '()
    (let [right (right-branch tree)
          left (left-branch tree)]
      (cond (contains-symbol? symbol right) (cons 1 (encode-symbol symbol right))
            (contains-symbol? symbol left) (cons 0 (encode-symbol symbol left))
            :else (throw (Exception. (str "bad symbol: ENCODE SYMBOL" symbol)))))))

(defn encode [message tree]
  (if (nil? message)
    '()
    (ch2/append (encode-symbol (car message) tree)
                (encode (cdr message) tree))))

;; 2.69
(defn successive-merge [leaves]
  (if (nil? (cdr leaves))
    (car leaves)
    (successive-merge (adjoin-huffman-set (make-code-tree (car leaves) (cadr leaves))
                                          (cddr leaves)))))

(defn generate-huffman-tree [pairs]
  (successive-merge (make-leaf-set pairs)))

;; 2.70
(def pairs '((A 2) (BOOM 1) (GET 2) (JOB 2) (SHA 3) (WAH 1) (NA 16) (YIP 9)))
(def h-tree (generate-huffman-tree pairs))
(def rock-song '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM))
(count  (encode rock-song h-tree))
;84
;alphabet size = 8 -> 3 bits per word
