(ns chapter2.part3.huffman-trees-test
  (:require [clojure.test :refer :all]
            [chapter2.part3.huffman-trees :refer :all]
            [chapter2.part3.huffman-trees-samples :as ch3]))

(deftest huffman-decode-test
  (testing "2.67 - Decoding a huffman tree"
    (is (= '(A D A B B C A)
           (ch3/decode sample-message sample-tree)))))
