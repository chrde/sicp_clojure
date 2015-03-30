(ns chapter2.part2.samples)

(defn car [l]
  (first l))

(defn cdr [l]
  (next l))

(defn cadr [l]
  (car (cdr l)))

(defn caddr [l]
  (car (cdr (cdr l))))

(defn cadddr [l]
  (car (cdr (cdr (cdr l)))))

(defn list-ref [l n]
  (if (zero? n)
    (car l)
    (list-ref (cdr l) (dec n))))

(defn length [l]
  (if (nil? (car l))
    0
    (inc (length (cdr l)))))

(defn append [l1 l2]
  (if (nil? l1)
    l2
    (cons (car l1) (append (cdr l1) l2))))

(defn map- [proc items]
  (if (nil? items)
    nil
    (cons (proc (car items))
          (map proc (cdr items)))))

(defn count-leaves [x]
  (cond (nil? x) 0
        (not (coll? x)) 1
        :else (+ (count-leaves (car x))
                 (count-leaves (cdr x)))))

(defn filter- [pred items]
  (cond (nil? items) nil
        (pred (car items)) (cons (car items)
                                 (filter- pred (cdr items)))
        :else (filter- pred (cdr items))))

(defn accumulate- [op initial items]
  (if (nil? items)
    initial
    (op (car items)
        (accumulate- op initial (cdr items)))))

(defn enumerate-interval [low high]
  (if (> low high)
    nil
    (cons low (enumerate-interval (inc low) high))))

(defn enumerate-tree [tree]
  (cond (nil? tree) nil
        (not (coll? tree)) (list tree)
        :else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree)))))

(defn flatmap [proc items]
  (accumulate- append nil (map- proc items)))


(defn remove- [item seq]
  (filter- (fn [x] (not (= x item))) seq))

(defn permutations [s]
  (if (nil? s)
    (list '())
    (flatmap (fn [x] (map (fn [p] (cons x p))
                          (permutations (remove- x s))))
             s)))
