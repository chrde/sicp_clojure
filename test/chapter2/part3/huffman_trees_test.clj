(ns chapter2.part3.huffman-trees-test
  (:require [clojure.test :refer :all]
            [chapter2.part3.huffman-trees :refer :all]
            [chapter2.part3.huffman-trees-samples :as ch3]))

(deftest huffman-decode-test
  (testing "2.67 - Decoding with huffman"
    (is (= '(A D A B B C A)
           (ch3/decode sample-message sample-tree)))))

(deftest huffman-encode-test
  (testing "2.68 - Encoding with huffman"
    (is (= sample-message
           (encode (ch3/decode sample-message sample-tree) sample-tree)))))
