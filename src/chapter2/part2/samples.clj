(ns chapter2.part2.samples)

(defn car [l]
  (first l))

(defn cdr [l]
  (next l))

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
    (cons (car l1) (append (cdr l1) l2))
    ))
