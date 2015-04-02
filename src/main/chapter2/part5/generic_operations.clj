(ns chapter2.part5.generic-operations
  (:require [chapter2.part2.samples :refer [car cdr]]
            [chapter2.part4.common-stuff :as common]))

;; TODO - after creating table in chapter 3, and defining put and get Operations

;; 2.78
(defn attach-tag [type-tag contents]
  (if (number? contents)
    contents
    (cons type-tag contents)))

(defn type-tag [datum]
  (cond (number? datum) :number
        (coll? datum) (car datum)
        :else (common/error "Bad tagged datum: TYPE-TAG" datum)))

(defn contents [datum]
  (cond (number? datum) datum
        (coll? datum) (cdr datum)
        :else (common/error ("Bas tagged datum: CONTENTS" datum))))
;; 2.79
;; 2.80
;; 2.81
