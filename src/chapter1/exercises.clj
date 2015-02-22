(ns chapter1.exercises)

;; 1.2
(/ (+ 5 1 (- 2 (- 3 (+ 6 (/ 1 3)))))
   (* 3 (- 6 2) (- 2 7)))
;; 1.3
(defn sqr-of-maxs [x y z]
  (let [a (min x y z)
        sqr #(* %1 %1)]
    (+ (sqr x)
       (sqr y)
       (sqr z)
       (- (sqr a)))))
;; 1.5
