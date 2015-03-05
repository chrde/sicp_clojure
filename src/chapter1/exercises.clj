(ns chapter1.exercises
  (:require [chapter1.samples :as ch1]))

;; 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

;; 1.3
(defn sqr-of-maxs [x y z]
  (let [a (min x y z)]
    (reduce + (conj
                (map ch1/sqr [x y z])
                (- (ch1/sqr a))))))

;; 1.7
(defn improved-enough? [x y]
  (< (ch1/abs (- x y)) 0.001))
(defn improved-sqr-iter
  ([guess n]
   (improved-sqr-iter guess n 0))
  ([guess n previous]
   (if (and (ch1/good-enough? guess n)
            (improved-enough? guess previous))
     guess
     (recur (ch1/improve guess n) n guess))))

;; 1.8
(defn cube [x]
  (Math/pow x 3))

(defn good-enough? [guess n]
  (< (ch1/abs (- (cube guess) n)) 0.001))

(defn improve-cube [guess n]
  (/ (+ (* 2 guess) (/ n (ch1/sqr guess)))
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
         (even? n) (iter-fast-expt (ch1/sqr b) (/ n 2) acum)
         :else (iter-fast-expt b (dec n) (* acum b)))))

;; 1.17
(defn doublee [n]
  (+ n n))
(defn halve [n]
  (/ n 2))

(defn recur-mult [x y]
  (cond (zero? y) 0
        (even? y) (doublee (recur-mult x (halve y)))
        :else (+ x (recur-mult x (dec y)))))

;; 1.18
(defn iter-mult
  ([x y] (iter-mult x y 0))
  ([x y acum]
   (cond (zero? y) acum
         (even? y) (iter-mult (doublee x) (halve y) acum)
         :else (iter-mult x (dec y) (+ acum x)))))

;; 1.19
(defn fib-iter
  ([n] (fib-iter 1 0 0 1 n))
  ([a b p q n]
   (cond (zero? n) b
         (even? n) (fib-iter a
                             b
                             (+ (ch1/sqr p) (ch1/sqr q))
                             (+ (* 2 p q) (ch1/sqr q))
                             (/ n 2))
         :else (fib-iter (+ (* b q) (* a q) (* a p))
                         (+ (* b p) (* a q))
                         p
                         q
                         (dec n)))))

;; 1.22
(defn prime? [n]
  [(ch1/prime? n) n])

(defn measure-time-of [f]
  (let [a (vector (System/nanoTime) (f) (System/nanoTime))]
    [(a 1) (- (a 2) (a 0))]))


(defn find-n-primes [start n]
  (->> (map #(+ start %) (range))
       (map #(fn [] (prime? %)))
       (map measure-time-of)
       (filter (comp true? first first))
       (take n)))

;; 1.23
(defn next-divisor [n]
  (if (= 2 n)
    3
    (+ 2 n)))

(defn find-divisor
  ([n] (find-divisor n 2))
  ([n test-divisor]
   (cond (> (ch1/sqr test-divisor) n) n
         (zero? (rem n test-divisor)) test-divisor
         :else (recur n (next-divisor test-divisor)))))

;; 1.24
(defn fermat-prime? [n]
  [(ch1/fast-prime? n 10) n])

(defn find-n-primes-fermat [start n]
  (->> (map #(+ start %) (range))
       (map #(fn [] (fermat-prime? %)))
       (map measure-time-of)
       (filter (comp true? first first))
       (take n)))

;; 1.27
(defn full-fermat-test [n]
  (->> (range n)
       (map #(ch1/try-it % n))
       (every? true?)))

;; 1.28
(defn non-trivial-sqr [x m]
  (let [square (rem (ch1/sqr x) m)]
    (if (and (not= 1 x)
             (not= x (dec m))
             (= 1 square))
      0
      square)))

(defn miller-rabin-expmod [base exp m]
  (cond (zero? exp) 1
        (even? exp) (non-trivial-sqr (miller-rabin-expmod base (/ exp 2) m) m)
        :else (rem (* base (miller-rabin-expmod base (dec exp) m)) m)))

(defn miller-rabin-test [n]
  (let [x (miller-rabin-expmod (inc (rand-int (dec n)))
                               (dec n)
                               n)]
    (= 1 x)))

(defn fast-prime-mr? [n times]
  (cond (zero? times) true
        (miller-rabin-test n) (fast-prime-mr? n (dec times))
        :else false))

;; 1.29
(defn sum [term a nextt b]
  (if (> a b)
    0
    (+ (term a)
       (sum term (nextt a) nextt b))))
(defn multiplier [k n]
  (cond (zero? k) 1
        (= k n) 1
        (even? k) 2
        :else 4))
(defn simpson-rule [f a b n]
  (letfn [(h [] (/ (- b a) n))
          (yk [k] (f (+ a (* (h) k))))
          (simpson-aprox [k] (* (multiplier k n) (yk k)))]
    (* (/ (h) 3) (sum simpson-aprox 0 inc n))))

;; 1.30
(defn iter-sum [term a nextt b]
  (letfn [(iteration [a result]
                     (if (> a b)
                       result
                       (iteration (nextt a) (+ result (term a)))))]
    (iteration a 0)))

;; 1.31
(defn recur-product [term a nextt b]
  (if (> a b)
    1
    (* (term a) (recur-product term (nextt a) nextt b))))

(defn factorial [n]
  (recur-product identity 1 inc n))

(defn iter-product [term a nextt b]
  (letfn [(iteration [a result]
                     (if (> a b)
                       result
                       (iteration (nextt a) (* result (term a)))))]
    (iteration a 1)))

;; 1.32
(defn accumulate [combiner null-value term a nextt b]
  (if (> a b)
    null-value
    (combiner (term a) (accumulate combiner null-value term (nextt a) nextt b))))

(defn accumulate-iter [combiner null-value term a nextt b]
  (letfn [(iteration [a result]
                     (if (> a b)
                       result
                       (iteration (nextt a) (combiner result (term a)))))]
    (iteration a null-value)))

;; 1.33
(defn filtered-accumulate [combiner null-value term a nextt b filterr]
  (let [value (if (filterr a)
                (term a)
                null-value)]
    (if (> a b)
      null-value
      (combiner value
                (filtered-accumulate combiner
                                     null-value
                                     term
                                     (nextt a)
                                     nextt
                                     b
                                     filterr)))))

(defn sum-of-square-of-primes [a b]
  (filtered-accumulate + 0 ch1/sqr a inc b ch1/prime?))

(defn relative-prime? [a b]
  (= 1 (ch1/gcd a b)))
(defn product-of-relative-primes [n]
  (letfn [(filterr [x] (relative-prime? n x))]
    (filtered-accumulate * 1 identity 1 inc n filterr)))

;; 1.35
(defn golden-ratio-approx [x]
  (+ 1 (/ 1 x)))

(defn golden-ratio [x]
  (ch1/fixed-point golden-ratio-approx x))

;; 1.36
(defn verbose-wrapper [approx-f arg]
  (let [y (approx-f arg)]
    (do (println y)
        y)))

(defn golden-ratio-verbose [x]
  (let [f (partial verbose-wrapper golden-ratio-approx)]
    (ch1/fixed-point f 1.0)))

(defn x-square-approx [x]
  (/ (Math/log 1000) (Math/log x)))

(defn x-square-verbose [x]
  (let [f (partial verbose-wrapper x-square-approx)]
    (ch1/fixed-point f x)))

(defn x-square-approx-avg [x]
  (ch1/average (Math/log 1000) (/ (Math/log 1000) (Math/log x))))

(defn x-square-verbose-avg [x]
  (let [f (partial verbose-wrapper x-square-approx-avg)]
    (ch1/fixed-point f x)))

;; 1.37
(defn cont-frac
  ([n d k]
   (cont-frac n d k 0.0))
  ([n d k acc]
   (if (zero? k)
     acc
     (cont-frac n d (dec k) (/ (n k) (+ (d k) acc))))))

(defn cont-frac-recur
  ([n d k]
   (cont-frac-recur n d k 1))
  ([n d k i]
   (if (> i k)
     0.0
     (/ (n k) (+ (d k) (cont-frac-recur n d k (inc i)))))))

;; 1.38
(defn d-eulers-expansion [i]
  (if (= 2 (mod i 3))
    (* 2 (inc (quot i 3)))
    1))

;; 1.39
(defn d-lambert [i]
  (dec (* 2 i)))

(defn n-lambert [x i]
  (if (= 1 i)
    x
    (ch1/sqr x)))

(defn tan-cf [x k]
  (cont-frac (partial n-lambert x)
             d-lambert
             k))

;; 1.40
(defn cubic [a b c]
  (fn [x] (+ (cube x)
             (* a (ch1/sqr x))
             (* b x)
             c)))

;; 1.41
(defn execute-twice [f]
  (fn [x] (f (f x))))

;; 1.42
(defn compose [f g]
  (fn [x] (f (g x))))

;; 1.43
(defn repeated [f n]
  (if (= 1 n)
    f
    (compose f (repeated f (dec n)))))

;; 1.44
(defn smooth [f]
  (let [dx 0.00001]
    (fn [x]
      (/ (+ (f (+ x dx))
            (f (- x dx))
            (f x))
         3))))

(defn n-fold-smoothed [f n]
  ((repeated smooth n) f))

;; 1.45
(defn nth-average [n]
  (repeated ch1/average-damp n))

(defn log2 [x]
  (/ (Math/log x) (Math/log 2)))

(defn nth-root [x n]
  (let [times (int (log2 n))]
    (ch1/fixed-point ((nth-average times)
                       (fn [y] (/ x (Math/pow y (dec n)))))
                     1.0)))

;; 1.46
(defn iterative-improve [good-enough? improve-guess]
  (fn [guess]
    (let [next (improve-guess guess)]
      (if (good-enough? guess next)
        next
        (recur next)))))

(defn sqr-iterative-improve [x]
  ((iterative-improve ch1/good-enough? (partial ch1/improve 1.0))
    x))

(defn fixed-point-iterative-improve [f first-guess]
  ((iterative-improve ch1/close-enough? f)
    first-guess))
