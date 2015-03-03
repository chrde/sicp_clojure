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
  (cond (zero? n) 0
        (= n 1) 1
        :else (+ (recur-fibonacci (- n 1))
                 (recur-fibonacci (- n 2)))))

(defn iter-fibonacci
  ([n]
   (iter-fibonacci 1 0 n))
  ([a b count]
   (if (zero? count)
     b
     (iter-fibonacci (+ a b) a (dec count)))))

;; Counting change
(def coin-values {1 1, 2 5, 3 10, 4 25, 5 50})

(defn count-change
  ([amount]
   (count-change amount 5))
  ([amount actual-coin]
   (cond (zero? amount) 1
         (or (< amount 0) (zero? actual-coin)) 0
         :else (+ (count-change amount (dec actual-coin))
                  (count-change (- amount (coin-values actual-coin))
                                actual-coin)))))

;; Exponentiation
(defn recur-expt [b n]
  (if (zero? n)
    1
    (* b (recur-expt b (dec n)))))

(defn iter-expt
  ([b n] (iter-expt b n 1))
  ([b n product]
   (if (zero? n)
     product
     (iter-expt b (dec n) (* b product)))))

(defn recur-fast-expt [b n]
  (cond (zero? n) 1
        (even? n) (sqr (recur-fast-expt b (/ n 2)))
        :else (* b (recur-fast-expt b (dec n)))))

;; Gcd
(defn gcd [a b]
  (if (zero? b)
    a
    (gcd b (rem a b))))

;; Basic prime testing
(defn find-divisor
  ([n] (find-divisor n 2))
  ([n test-divisor]
   (cond (> (sqr test-divisor) n) n
         (zero? (rem n test-divisor)) test-divisor
         :else (recur n (inc test-divisor)))))

(defn prime? [n]
  (= n (find-divisor n)))

;;Fermat prime test
(defn expmod [base exp m]
  (cond (zero? exp) 1
        (even? exp) (rem (sqr (expmod base (/ exp 2) m)) m)
        :else (rem (* base (expmod base (dec exp) m)) m)))

(defn try-it [a n]
  (= (expmod a n n) a))

(defn fermat-test [n]
  (try-it (inc (rand-int (dec n))) n))

(defn fast-prime? [n times]
  (cond (zero? times) true
        (fermat-test n) (fast-prime? n (dec times))
        :else false))

;; Half-interval method
(defn close-enough? [x y]
  (< (abs (- x y)) 0.000001))

(defn find-desired-midpoint [f neg-point pos-point]
  (let [mid-point (average neg-point pos-point)
        mid-value (f mid-point)]
    (if (close-enough? neg-point pos-point)
      mid-point
      (cond (pos? mid-value) (find-desired-midpoint f neg-point mid-point)
            (neg? mid-value) (find-desired-midpoint f mid-point pos-point)
            :else mid-point))))

(defn half-interval [f a b]
  (let [a-value (f a)
        b-value (f b)]
    (cond (and (neg? a-value) (pos? b-value)) (find-desired-midpoint f a b)
          (and (neg? b-value) (pos? a-value)) (find-desired-midpoint f b a)
          :else "Error: Values are not of opposite sign")))

;; Fixed points
(defn fixed-point [f guess]
  (let [nextt (f guess)]
    (if (close-enough? nextt guess)
      nextt
      (fixed-point f nextt))))

(defn sqrt-fixed-point [x]
  (fixed-point #(average % (/ x %)) 1.0))
