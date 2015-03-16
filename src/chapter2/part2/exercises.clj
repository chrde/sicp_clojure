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
(defn parity [x]
  (cond (zero? x) (fn [_] (and false _))
        (even? x) even?
        :else odd?))

(defn add-same-parity [f l]
  (cond (nil? (ch2/car l)) '()
        (f (ch2/car l)) (cons (ch2/car l) (add-same-parity f (ch2/cdr l)))
        :else (add-same-parity f (ch2/cdr l))))

(defn same-parity [& args]
  (if (nil? args)
    '()
    (add-same-parity (parity (ch2/car args))
                     args)))

;; 2.21
(defn square-list-recur [l]
  (if (nil? l)
    '()
    (cons (chapter1.samples/sqr (ch2/car l)) (square-list-recur (ch2/cdr l)))))

(defn square-list [l]
  (ch2/map- chapter1.samples/sqr l))

;; 2.23
(def for-each ch2/map-)

;; 2.25
(defn first-car-cdr [l]
  (-> l
      ch2/cdr
      ch2/cdr
      ch2/car
      ch2/cdr
      ch2/car))

(defn second-car-cdr [l]
  (-> l
      ch2/car
      ch2/car))

(defn third-car-cdr [l]
  (-> l
      ch2/cdr
      ch2/car
      ch2/cdr
      ch2/car
      ch2/cdr
      ch2/car
      ch2/cdr
      ch2/car
      ch2/cdr
      ch2/car
      ch2/cdr
      ch2/car
      ))

;; 2.27
(defn deep-reverse
  ([l] (deep-reverse l (empty l)))
  ([l r-l]
   (cond (nil? (ch2/car l)) r-l
         (not (coll? (ch2/car l))) (deep-reverse (ch2/cdr l) (cons (ch2/car l) r-l))
         :else (deep-reverse (ch2/cdr l) (cons (deep-reverse (ch2/car l)) r-l)))))

(defn deep-reverse- [l]
  (if (coll? l)
    (reverse- (ch2/map- deep-reverse- l))
    l))

;; 2.28
(defn fringe
  ([l] (fringe l '()))
  ([l r-l]
   (cond (nil? l) r-l
         (not (coll? l)) (cons l r-l)
         :else (fringe (ch2/car l) (fringe (ch2/cdr l) r-l)))))

;; 2.29
(defn make-mobile [left right]
  (list left right))

(defn make-branch [length structure]
  (list length structure))

(defn left-branch [mobile]
  (ch2/car mobile))

(defn right-branch [mobile]
  (ch2/car (ch2/cdr mobile)))

(defn branch-length [branch]
  (ch2/car branch))

(defn branch-structure [branch]
  (ch2/car (ch2/cdr branch)))

(defn total-weight [structure]
  (if (coll? structure)
    (+ (total-weight (branch-structure (left-branch structure)))
       (total-weight (branch-structure (right-branch structure))))
    structure))

(defn torque [branch]
  (* (branch-length branch)
     (total-weight (branch-structure branch))))

(defn balanced-structure? [structure]
  (if (not (coll? structure))
    true
    (let [right (right-branch structure)
          left (left-branch structure)]
      (and (= (torque left)
              (torque right))
           (balanced-structure? (branch-structure left))
           (balanced-structure? (branch-structure right))))))

;; 2.30
(defn square-tree [tree]
  (cond (nil? tree) nil
        (not (coll? tree)) (chapter1.samples/sqr tree)
        :else (cons (square-tree (ch2/car tree))
                    (square-tree (ch2/cdr tree)))))

;; 2.31
(defn tree-map [f tree]
  (ch2/map- (fn [sub-tree]
              (if (coll? sub-tree)
                (tree-map f sub-tree)
                (f sub-tree)))
            tree))

(defn tree-map1 [f tree]
  (cond (nil? tree) nil
        (not (coll? tree)) (f tree)
        :else (cons (tree-map1 f (ch2/car tree))
                    (tree-map1 f (ch2/cdr tree)))))

;; 2.32
(defn subsets [s]
  (if (nil? s)
    (list '())
    (let [rest (subsets (ch2/cdr s))]
      (ch2/append rest (map (fn [x] (cons (ch2/car s) x))
                            rest)))))

;; 2.33
(defn map-acc [p sec]
  (ch2/accumulate- (fn [x y] (cons (p x) y))
                   nil
                   sec))

(defn append-acc [seq1 seq2]
  (ch2/accumulate- cons seq2 seq1 ))

(defn length-acc [seq]
  (ch2/accumulate- (fn [_ y] (inc y)) 0 seq))
