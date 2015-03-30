(ns chapter2.part3.huffman-trees-test
  (:require [clojure.test :refer :all]
            [chapter2.part3.huffman-trees :refer :all]
            [chapter2.part3.huffman-trees-samples :as ch3]))

(deftest decode-test
  (testing "2.67 - Decoding with huffman"
    (is (= '(A D A B B C A)
           (ch3/decode sample-message sample-tree)))))

(deftest encode-test
  (testing "2.68 - Encoding with huffman"
    (is (= sample-message
           (encode (ch3/decode sample-message sample-tree) sample-tree)))))

(deftest generate-huffman-tree-test
  (testing "2.69 - Generating huffman tree"
    (is (= sample-tree
           (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1)))))))
