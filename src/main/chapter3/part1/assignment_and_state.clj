(ns chapter3.part1.assignment-and-state)

;; 3.1
(defn make-accumulator [initial-value]
  (let [value (atom initial-value)]
    (fn [amount]
      (swap! value + amount))))

;; 3.2
(defn make-monitored [f]
  (let [count (atom 0)]
    (fn [arg]
      (cond (= :how-many-calls arg) @count
            (= :reset-count arg) (reset! count 0)
            :else (do (swap! count inc)
                      (f arg))))))
