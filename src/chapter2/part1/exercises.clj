(ns chapter2.part1.exercises
  (:require [chapter2.part1.samples :as ch2.1]
            [chapter1.samples :as ch1]))

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
