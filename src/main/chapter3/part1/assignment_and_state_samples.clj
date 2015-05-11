(ns chapter3.part1.assignment-and-state-samples)

(defn error [& args]
  (throw (Exception. (apply str args))))

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
          (deposit [amount]
            (swap! balance + amount))
          (dispatch [m]
            (cond (= m :withdraw) withdraw
                  (= m :deposit) deposit
                  :else (error "Unknown request: MAKE-ACCOUNT" m)))]
    dispatch))

(def acc (make-account (atom 100)))
((acc :withdraw) 50)
((acc :withdraw) 60)
((acc :deposit) 40)
((acc :withdraw) 60)
