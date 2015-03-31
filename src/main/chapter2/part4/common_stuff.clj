(ns chapter2.part4.common-stuff
  (:require [chapter2.part2.samples :refer [car cdr]]))

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
  (cons type-tag contents))

(defn type-tag [datum]
  (if (coll? datum)
    (car datum)
    (error "Bad tagged datum: TYPE-TA" datum)))

(defn contents [datum]
  (if (coll? datum)
    (cdr datum)
    (error "Bad tagged datum: CONTENTS" datum)))
