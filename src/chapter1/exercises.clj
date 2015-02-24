(ns chapter1.exercises
  (:require [chapter1.samples :as chp1]))

;; 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

;; 1.3
(defn sqr-of-maxs [x y z]
  (let [a (min x y z)]
    (reduce + (conj
                (map chp1/sqr [x y z])
                (- (chp1/sqr a))))))

;; 1.7
(defn improved-enough? [x y]
  (< (chp1/abs (- x y)) 0.001))
(defn improved-sqr-iter
  ([guess n]
   (improved-sqr-iter guess n 0))
  ([guess n previous]
   (if (and (chp1/good-enough? guess n)
            (improved-enough? guess previous))
     guess
     (recur (chp1/improve guess n) n guess))))

;; 1.8
(defn cube [x]
  (Math/pow x 3))

(defn good-enough? [guess n]
  (< (chp1/abs (- (cube guess) n)) 0.001))

(defn improve-cube [guess n]
  (/ (+ (* 2 guess) (/ n (chp1/sqr guess)))
     3))

(defn cube-iter [guess n]
  (if (good-enough? guess n)
    guess
    (recur (improve-cube guess n) n)))

;; 1.11
(defn recur-f [n]
  (if (< n 3)
    n
    (+ (recur-f (- n 1))
       (* 2 (recur-f (- n 2)))
       (* 3 (recur-f (- n 3))))))

(defn iter-f
  ([n]
   (if (< n 3)
     n
     (iter-f 0 1 2 (- n 3))))
  ([x y z n]
   (if (zero? n)
     (+ z (* 2 y) (* 3 x))
     (iter-f y z (+ z (* 2 y) (* 3 x)) (dec n)))))

;; 1.12
;;First way - each element
(defn pascal-element [row col]
  (cond (= col 1) 1
        (= col row) 1
        :else (+ (pascal-element (dec row) (dec col))
                 (pascal-element (dec row) col))))
;;Second way - each row
(defn next-line [line]
  (let [line (concat '(0) line '(0))]
    (map #(apply + %) (partition 2 1 line))))

(defn pascal-line [n]
  (let [f (iterate next-line (list n))]
    (nth f n)))

;; 1.16
(defn iter-fast-expt
  ([b n] (iter-fast-expt b n 1))
  ([b n acum]
    (cond (zero? n) acum
          (even? n) (iter-fast-expt (chp1/sqr b) (/ n 2) acum)
          :else (iter-fast-expt b (dec n) (* acum b)))))
