(ns chapter2.part1.exercises
  (:require [chapter2.part1.samples :as ch2.1]
            [chapter1.samples :as ch1]
            [chapter1.exercises :as ch1e]))

;; 2.1
;;ch2.1/make-rat

;; 2.2
(defn make-point [x y]
  [x y])
(defn x-point [p]
  (first p))
(defn y-point [p]
  (second p))
(defn print-point [p]
  (println (str "(" (x-point p) "," (y-point p) ")")))

(defn make-segment [start-point end-point]
  [start-point end-point])
(defn start-segment [s]
  (first s))
(defn end-segment [s]
  (second s))

(defn midpoint-segment [s]
  (let [p1 (start-segment s)
        p2 (end-segment s)]
    (make-point (ch1/average (x-point p1) (x-point p2))
                (ch1/average (y-point p1) (y-point p2)))))

;; 2.3
(defn difference [f p1 p2]
  (ch1/abs (- (ch1/abs (f p1))
              (ch1/abs (f p2)))))
(def x-difference (partial difference x-point))
(def y-difference (partial difference y-point))

(defn length-segment [s]
  (let [x-length (x-difference (start-segment s) (end-segment s))
        y-length (y-difference (start-segment s) (end-segment s))]
    (ch1/newton-method-sqrt (+ (ch1/sqr x-length)
                               (ch1/sqr y-length)))))

(defn make-rectangle [top-seg right-seg down-seg left-seg]
  [top-seg right-seg down-seg left-seg])

(defn perimeter-rectangle [r]
  (+ (length-segment (r 0))
     (length-segment (r 1))
     (length-segment (r 2))
     (length-segment (r 3))))

(defn area-rectangle [r]
  (* (length-segment (r 0))
     (length-segment (r 1))))

;; 2.4
(defn conss [x y]
  (fn [m] (m x y)))

(defn car [z]
  (z (fn [p _] p)))

(defn cdr [z]
  (z (fn [_ q] q)))

;; 2.5
(defn remove-mult-of
  ([x mult] (remove-mult-of x mult 0))
  ([x mult acc]
   (let [next-x (quot x mult)]
     (if (zero? (rem x mult))
       (remove-mult-of next-x mult (inc acc))
       [x acc]))))

(defn cons-exp [a b]
  (* (Math/pow 2 a)
     (Math/pow 3 b)))

(defn car-exp [x]
  (let [[y z] (remove-mult-of x 6)
        [_ n] (remove-mult-of y 2)]
    (+ n z)))

(defn cdr-exp [x]
  (let [[y z] (remove-mult-of x 6)
        [_ n] (remove-mult-of y 3)]
    (+ n z)))

;; 2.6
(defn zero [f]
  (fn [x] x))

(defn church-next [n]
  (fn [f]
    (fn [x]
      (f ((n f) x)))))

(defn one [f]
  (fn [x] (f x)))

(defn two [f]
  (fn [x] (f (f x))))

(defn church+ [a b]
  (fn [f]
    (fn [x]
      ((b f) ((a f) x)))))

(defn church* [a b]
  (fn [f]
    (fn [x]
      ((b (a f)) x))))

;; 2.7
(defn make-interval [lower-bound upper-bound]
  (conss lower-bound upper-bound))

(defn lower-bound [interval]
  (min (car interval)
       (cdr interval)))

(defn upper-bound [interval]
  (max (car interval)
       (cdr interval)))

(defn add-interval [x y]
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(defn mul-interval [x y]
  (let [p1 (* (lower-bound x) (lower-bound y))
        p2 (* (lower-bound x) (upper-bound y))
        p3 (* (upper-bound x) (upper-bound y))
        p4 (* (upper-bound x) (lower-bound y))]
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(defn div-interval [x y]
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

;; 2.8
(defn sub-interval [x y]
  (add-interval x
                (make-interval (- (lower-bound y))
                               (- (upper-bound y)))))

;; 2.10
(defn product [interval]
  (* (car interval)
     (cdr interval)))

(defn spans-zero? [interval]
  (<= (product interval) 0))

;; 2.12
(defn center [i]
  (/ (+ (lower-bound i)
        (upper-bound i))
     2))

(defn make-center-percent [c p]
  (let [percent (/ (* p c) 100)]
    (make-interval (- c percent)
                   (+ c percent))))

(defn percent [i]
  (let [c (center i)]
    (* 100 (dec (/ c (lower-bound i))))))
