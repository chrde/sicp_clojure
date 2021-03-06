(ns chapter2.part5.symbolic-algebra-samples
  (:require [chapter2.part3.symbolic-differentiation-samples :refer [same-variable?]]
            [chapter2.part2.samples :refer [car cadr]]
            [chapter2.part4.common-stuff :refer [error]]
            [chapter2.part4.operations-table :as table]))

;(defn make-poly [variable terms]
;  (list variable terms))
;
;(defn variable [poly]
;  (car poly))
;
;(defn terms [poly]
;  (cadr poly))
;
;(defn add-poly [p1 p2]
;  (if (same-variable? (variable p1) (variable p2))
;    (make-poly (variable p1)
;               (add-terms (term-list p1) (term-list p2)))
;    (error "Polys not in same var: ADD-POLY" (list p1 p2))))
;
;(defn mul-poly [p1 p2]
;  (if (same-variable? (variable p1) (variable p2))
;    (make-poly (variable p1)
;               (mul-terms (term-list p1) (term-list p2)))
;    (error "Polys not in same var: MUL-POLY" (list p1 p2))))
;
;(defn adjoin-term [term term-list]
;  (if (=zero? (coeff term))
;    term-list
;    (cons term term-list)))
;
;(defn the-empty-termlist []
;  '())
;(defn first-term [term-list]
;  (car term-list))
;(defn rest-terms [term-list]
;  (cdr term-list))
;(defn empty-termlist? [term-list]
;  (nil? term-list))
;(defn make-term [order coeff]
;  (list order coeff))
;(defn order [term]
;  (car term))
;(defn coeff [term]
;  (cadr term))
;
;(defn add-terms [L1 L2]
;  (cond (empty-termlist? L1) L2
;        (empty-termlist? L2) L1
;        :else (let [t1 (first-term L1)
;                    t2 (first-term L2)]
;                (cond (> (order t1) (order t2)) (adjoin-term t1 (add-terms (rest-terms L1) L2))
;                      (< (order t1) (order t2)) (adjoin-term t2 (add-terms L1 (rest-terms L2)))
;                      :else (adjoin-term (make-term (order t1)
;                                                    (add (coeff t1) (coeff t2)))
;                                         (add-terms (rest-terms L1) (rest-terms L2)))))))
;
;(defn mul-terms [L1 L2]
;  (if (empty-termlist? L1)
;    (the-empty-termlist)
;    (add-terms (mul-term-by-all-terms (first-term L1) L2)
;               (mul-terms (rest-terms L1) L2))))
;
;(defn mul-term-by-all-terms [t1 L]
;  (if (empty-termlist? L)
;    (the-empty-termlist)
;    (let [t2 (first-term L)]
;      (adjoin-term
;       (make-term (+ (order t1) (order t2))
;                  (mul (coeff t1) (coeff t2)))
;       (mul-term-by-all-terms t1 (rest-terms L))))))
;
;(defn install-polynomial-package []
;  ;; internal procedures
;  ;; representation of poly
;  (defn make-poly [variable term-list] (cons variable term-list))
;  (defn variable [p] (car p))
;  (defn term-list [p] (cdr p))
;  ; procedures same-variable? and variable? from section 2.3.2 ⟩
;  ;; representation of terms and term lists
;  ; procedures adjoin-term .. . coeff from text below ⟩
;  (defn add-poly [p1 p2] .. .)
;  ; procedures used by add-poly ⟩
;  (defn mul-poly [p1 p2] .. .)
;  ; procedures used by mul-poly ⟩
;  ;; interface to rest of the system
;  (defn tag [p] (attach-tag 'polynomial p))
;  (put 'add '(polynomial polynomial)
;       (lambda (p1 p2) (tag (add-poly p1 p2))))
;  (put 'mul '(polynomial polynomial)
;       (lambda (p1 p2) (tag (mul-poly p1 p2))))
;  (put 'make 'polynomial
;       (lambda (var terms) (tag (make-poly var terms))))
;  'done)
