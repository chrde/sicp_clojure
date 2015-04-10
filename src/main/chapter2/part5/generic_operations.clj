(ns chapter2.part5.generic-operations
  (:require [chapter2.part2.samples :refer [car cdr map- filter- cadr length accumulate-]]
            [chapter2.part4.common-stuff :as common]
            [chapter2.part4.complex-numbers-data-directed :as compl]
            [chapter2.part1.samples :as rat]
            [chapter2.part4.operations-table :as table]))

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
        :else (common/error ("Bad tagged datum: CONTENTS" datum))))
;; 2.79
(defn equ?-number [x y]
  (= x y))

(defn equ?-rational [x y]
  (= (* (rat/numer x) (rat/denom y)) (* (rat/numer y) (rat/denom x))))

(defn equ?-complex [x y]
  (and (= (compl/real-part x) (compl/real-part y)) (= (compl/imag-part x) (compl/imag-part y))))

(defn install-equ?-package []
  (do (table/put :equ '(:number :number) equ?-number)
      (table/put :equ '(:complex :complex) equ?-complex)
      (table/put :equ '(:rational :rational) equ?-rational)))

(defn equ? [x y]
  (with-redefs [common/attach-tag attach-tag
                common/type-tag type-tag
                common/contents contents]
    (compl/apply-generic :equ x y)))

(install-equ?-package)
;; 2.80
(defn zero?-number [x]
  (zero? x))

(defn zero?-rational [x]
  (zero? (rat/denom x)))

(defn zero?-complex [x]
  (= 0 (compl/imag-part x) (compl/imag-part x)))

(defn install-zero?-package []
  (do (table/put :zero '(:number) zero?-number)
      (table/put :zero '(:complex) zero?-complex)
      (table/put :zero '(:rational) zero?-rational)))

(defn zero?- [x]
  (with-redefs [common/attach-tag attach-tag
                common/type-tag type-tag
                common/contents contents]
    (compl/apply-generic :zero x)))

(install-zero?-package)
;; 2.81
(defn apply-generic-coercion- [operation & args]
  (let [type-tags (map- common/type-tag args)
        proc (table/get operation type-tags)]
    (if proc
      (apply proc (map- common/contents args))
      (if (= (length args) 2)
        (let [type1 (car type-tags)
              type2 (cadr type-tags)
              a1 (car args)
              a2 (cadr args)
              t1->t2 (table/get-coercion type1 type2)
              t2->t1 (table/get-coercion type2 type1)]
          (cond (= type1 type2) (common/error "No method for these types -no coercion" (list operation type-tags))
                t1->t2 (apply-generic-coercion- operation (t1->t2 a1) a2)
                t2->t1 (apply-generic-coercion- operation a1 (t2->t1 a2))
                :else (common/error "No method for these types" (list operation type-tags))))))))

(defn install-coercion-package []
  (do (table/put-coercion identity :number :number)
      (table/put-coercion (comp cadr identity) :super-number :number)))

(install-coercion-package)

;; 2.82
(defn find-coercions-from-type [type other-types]
  (let [coercions (map- (fn [type2] (table/get-coercion type2 type)) other-types)]
    (if (some nil? coercions)
      '()
      coercions)))

(defn find-common-coercion [types]
  (car (filter- (comp not empty?) (map- (fn [type] (find-coercions-from-type type types)) types))))

(defn apply-generic-smart-coercion [operation args]
  (let [type-tags (map- common/type-tag args)
        proc (table/get operation type-tags)]
    (if proc
      (apply proc (map- common/contents args))
      (let [coercions (find-common-coercion type-tags)]
        (if coercions
          (apply-generic-smart-coercion operation (map #(%1 %2) coercions args))
          (common/error "Automatic coercion is not possible for these types" type-tags))))))

;; 2.83
(defn contents- [x]
  (cadr x))

(defn integer->rational [n]
  (common/attach-tag :rational (rat/make-rat n 1)))

(defn rational->real [r]
  (common/attach-tag :real (double (/ (rat/numer r) (rat/denom r)))))

(defn real->imag [r]
  (common/attach-tag :imaginarium (compl/make-from-real-imag r 0)))

(defn install-raise-number-package []
  (do (table/put 'raise :integer integer->rational)
      (table/put 'raise :rational rational->real)
      (table/put 'raise :real real->imag)))

(install-raise-number-package)

(defn raise [n]
  ((table/get 'raise (common/type-tag n)) (contents- n)))
;; 2.84

(defn install-number-coercions-package []
  (do (table/put-coercion integer->rational :integer :rational)
      (table/put-coercion rational->real :rational :real)
      (table/put-coercion real->imag :real :imaginarium)))

(install-number-coercions-package)

(defn coerce-to [x y]
  (let [coerc-fn (table/get-coercion  (common/type-tag x) (common/type-tag y))]
    (if coerc-fn
      (coerc-fn (contents- x)))))

(defn is-subtype? [x y]
  (let [type1 (common/type-tag x)
        type2 (common/type-tag y)]
    (cond (table/get-coercion type1 type2) true
          (table/get 'raise type1) (is-subtype? ((table/get 'raise type1) x)))))

(coerce-to (common/attach-tag :integer 5) (common/attach-tag :rational (integer->rational 4)))
