(ns chapter1.samples)

;; Square roots by newton's method
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

;; Factorial
(defn recur-factorial [n]
  (if (= n 1)
    1
    (* n (recur-factorial (dec n)))))

(defn iter-factorial
  ([n] (iter-factorial 1 1 n))
  ([acum current n]
    (if (> current n)
      acum
      (recur (* current acum) (inc current) n))))

;; Fibonacci
(defn recur-fibonacci [n]
  (cond (= n 0) 0
        (= n 1) 1
        :else (+ (recur-fibonacci (- n 1))
                 (recur-fibonacci (- n 2)))))

(defn iter-fibonacci
  ([n]
   (iter-fibonacci 1 0 n))
  ([a b count]
    (if (= count 0)
      b
      (iter-fibonacci (+ a b) a (dec count)))))

;; Counting change
(def coin-values {1 1, 2 5, 3 10, 4 25, 5 50})

(defn count-change
  ([amount]
   (count-change amount 5))
  ([amount actual-coin]
    (cond (= amount 0) 1
          (or (< amount 0) (= actual-coin 0)) 0
          :else (+ (count-change amount (dec actual-coin))
                   (count-change (- amount (coin-values actual-coin))
                                 actual-coin)))))
