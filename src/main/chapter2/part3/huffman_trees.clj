(ns chapter2.part3.huffman-trees
  (:require [chapter2.part3.huffman-trees-samples :refer :all]))

;; 2.67
(def sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(def sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree (make-leaf 'B 2)
                                  (make-code-tree (make-leaf 'D 1)
                                                  (make-leaf 'C 1)))))
