(ns chapter2.part5.refactored-generic-arithmetic.common
  (:require [chapter2.part2.samples :refer [car cadr]]))

;; Basic trigonometric functions
(defn sqrt [x]
  (Math/sqrt x))

(defn atan [x]
  (Math/atan [x]))

(defn cos [x]
  (Math/cos x))

(defn sin [x]
  (Math/sin [x]))

(def eq? =)

(defn error [& args]
  (throw (Exception. (apply str args))))

;; Tagging functions
(defn attach-tag [type-tag contents]
  (list type-tag contents))

(defn type-tag [datum]
  (if (coll? datum)
    (car datum)
    (error "Bad tagged datum: TYPE-TAG" datum)))

(defn contents [datum]
  (if (coll? datum)
    (cadr datum)
    (error "Bad tagged datum: CONTENTS" datum)))
