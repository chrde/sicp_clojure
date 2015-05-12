(ns chapter3.part1.assignment-and-state)

;; 3.1
(defn make-accumulator [initial-value]
  (let [value (atom initial-value)]
    (fn [amount]
      (swap! value + amount))))
