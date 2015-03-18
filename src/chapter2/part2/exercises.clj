(ns chapter2.part2.exercises
  (require [chapter2.part2.samples :as ch2]
           [chapter1.samples :as ch1]))

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
  (ch2/accumulate- cons seq2 seq1))

(defn length-acc [seq]
  (ch2/accumulate- (fn [_ y] (inc y)) 0 seq))

;; 2.34
(defn horner-eval [x coef-seq]
  (ch2/accumulate- (fn [coef higher-terms]
                     (+ coef (* x higher-terms)))
                   0
                   coef-seq))

;; 2.35
(defn count-leaves-acc [tree]
  (ch2/accumulate- +
                   0
                   (ch2/map- (fn [x] (if (coll? x) (count-leaves-acc x) 1))
                             tree)))

;; 2.36
(defn accumulate-n [op init seqs]
  (if (nil? (ch2/car seqs))
    nil
    (cons (ch2/accumulate- op init (ch2/map- ch2/car seqs))
          (accumulate-n op init (ch2/map- ch2/cdr seqs)))))

;; 2.37
(defn dot-product [v w]
  (ch2/accumulate- + 0 (map * v w)))

(defn matrix-*-vector [m v]
  (ch2/map- (fn [w] (dot-product w v))
            m))

(defn transpose [m]
  (accumulate-n cons '() m))

(defn matrix-*-matrix [m n]
  (let [cols (transpose n)]
    (map (fn [w] (matrix-*-vector cols w)) m)))

;; 2.38
(defn fold-right [op initial items]
  (if (nil? items)
    initial
    (op (ch2/car items)
        (fold-right op initial (ch2/cdr items)))))

(defn fold-left [op initial items]
  (if (nil? items)
    initial
    (fold-left op
               (op initial (ch2/car items))
               (ch2/cdr items))))

;; 2.39
(defn reverse-fold-left [l]
  (fold-left (fn [x y] (cons y x)) nil l))

(defn reverse-fold-right [l]
  (fold-right (fn [x y] (ch2/append y (list x))) nil l))

;; 2.40
(defn unique-pairs [n]
  (ch2/flatmap (fn [i] (map (fn [j] (list i j))
                            (ch2/enumerate-interval 1 (dec i))))
               (ch2/enumerate-interval 2 n)))

;; 2.41
(defn find-third-number [n]
  (fn [pair]
    (when-let [x (ch2/car pair)]
      (let [z (- n (ch2/car pair) (ch2/cadr pair))]
        (if (or (neg? z) (>= x z))
          '()
          (cons z pair))))))

(defn find-triples [n]
  (ch2/accumulate- (fn [pair acc]
                     (let [x ((find-third-number n) pair)]
                       (if (nil? (ch2/car x))
                         acc
                         (cons x acc))))
                   '()
                   (unique-pairs n)))

(defn find-triples-flatmap [n]
  (ch2/filter- (fn [pair] (= (ch2/accumulate- + 0 pair) n))
               (ch2/flatmap (fn [i]
                              (ch2/flatmap (fn [j]
                                             (ch2/map- (fn [k] (list i j k))
                                                       (ch2/enumerate-interval 1 (dec j))))
                                           (ch2/enumerate-interval 1 (dec i))))
                            (ch2/enumerate-interval 1 n))))

;; 2.42
(defn place-queen [row col]
  (list row col))

(defn row [queen]
  (ch2/car queen))

(defn col [queen]
  (ch2/cadr queen))

(defn collides? [q1 q2]
  (let [c (- (col q1) (col q2))
        r (- (row q1) (row q2))]
    (or (zero? r)
        (= (ch1/abs c) (ch1/abs r)))))

(defn safe? [_ queen-positions]
  (->> (ch2/cdr queen-positions)
       (ch2/map- (fn [queen] (collides? queen (ch2/car queen-positions))))
       (ch2/accumulate- (fn [x y] (or x y)) false)
       not))

(defn adjoin-position [row column rest-of-queens]
  (cons (place-queen row column) rest-of-queens))

(defn empty-board []
  (list))

(defn queens
  ([board-size] (queens board-size board-size))
  ([board-size k]
   (if (zero? k)
     (empty-board)
     (ch2/filter- (fn [positions] (safe? k positions))
                  (ch2/flatmap (fn [rest-of-queens]
                                 (ch2/map- (fn [new-row] (adjoin-position new-row
                                                                          k
                                                                          rest-of-queens))
                                           (ch2/enumerate-interval 1 board-size)))
                               (queens board-size (dec k)))))))

;; 2.44
(defn below [& _])
(defn beside [& _])
(defn up-split [painter n]
  (if (zero? n)
    painter
    (let [smaller (up-split painter (dec n))]
      (below painter (beside smaller smaller)))))

;; 2.45
(defn split [pos1 pos2]
  (fn f [painter n]
    (if (zero? n)
      painter
      (let [smaller (f painter (dec n))]
        (pos1 painter (pos2 smaller smaller))))))

;; 2.46
(defn make-vector [x y]
  (list x y))

(defn xcor-vec [v]
  (ch2/car v))

(defn ycor-vec [v]
  (ch2/cadr v))

(defn add-vec [v1 v2]
  (make-vector (+ (xcor-vec v1) (xcor-vec v2))
               (+ (ycor-vec v1) (ycor-vec v2))))

(defn sub-vec [v1 v2]
  (add-vec v1 (make-vector (- (xcor-vec v2))
                           (- (ycor-vec v2)))))

(defn scale-vect [v x]
  (make-vector (* (xcor-vec v) x)
               (* (ycor-vec v) x)))

;; 2.47
(defn make-frame1 [origin edge1 edge2]
  (list origin edge1 edge2))

(defn origin-frame1 [frame]
  (ch2/car frame))

(defn edge1-frame1 [frame]
  (ch2/cadr frame))

(defn edge2-frame1 [frame]
  (ch2/car (ch2/cdr (ch2/cdr frame))))


(defn make-frame2 [origin edge1 edge2]
  (cons origin (cons edge1 edge2)))

(defn origin-frame2 [frame]
  (ch2/car frame))

(defn edge1-frame2 [frame]
  (ch2/cadr frame))

(defn edge2-frame2 [frame]
  (ch2/cdr (ch2/cdr frame)))
