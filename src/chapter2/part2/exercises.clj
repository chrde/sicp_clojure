(ns chapter2.part2.exercises
  (require [chapter2.part2.samples :as ch2]))

;; 2.17
(defn last-pair [l]
  (if (nil? (ch2/cdr l))
    (ch2/car l)
    (last-pair (ch2/cdr l))))

;; 2.18
(defn reverse-
  ([l] (reverse- l (empty l)))
  ([l r-l]
    (if (nil? (ch2/car l))
      r-l
      (reverse- (ch2/cdr l) (cons (ch2/car l) r-l)))))

;; 2.19
(defn no-more? [l]
  (zero? (ch2/length l)))

(defn except-first-denomination [coin-values]
  (ch2/cdr coin-values))

(defn first-denomination [coin-values]
  (ch2/car coin-values))

(defn count-change [amount coin-values]
  (cond (zero? amount) 1
        (or (< amount 0) (no-more? coin-values)) 0
        :else (+ (count-change amount (except-first-denomination coin-values))
                   (count-change (- amount (first-denomination coin-values)) coin-values))))

;; 2.20
