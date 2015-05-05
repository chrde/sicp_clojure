(ns chapter3.part1.assignment-and-state-samples)

(defn new-withdraw []
  (let [balance (atom 100)]
    (fn [amount]
      (if (>= @balance amount)
        (swap! balance - amount)
        "Insufficient funds"))))

(def w1 (new-withdraw))
(w1 31)

(defn make-withdraw [balance]
  (fn [amount]
    (if (>= @balance amount)
      (swap! balance - amount)
      "Insufficient funds")))

(def w2 (make-withdraw (atom 100)))
(w2 12)

(defn make-account [balance]
  (letfn [(withdraw [amount]
            (if (>= @balance amount)
              (swap! balance - amount)
              "Insufficient funds"))
          (deposit [m]
            (cond (= m :withdraw) withdraw
                  (= m :deposit) deposit
                  :else ))]))
