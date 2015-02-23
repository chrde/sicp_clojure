(ns chapter1.samples)

;;Square roots by newton's method
(defn sqr [x]
  (* x x))

(defn abs [x]
  (if (> x 0)
    x
    (- x)))

(defn average [x y]
  (/ (+ x y) 2))

(defn improve [guess n]
  (average guess (/ n guess)))


(defn good-enough? [guess n]
  (< (abs (- (sqr guess) n)) 0.001))

(defn sqrt-iter [guess n]
  (if (good-enough? guess n)
    guess
    (recur (improve guess n) n)))

