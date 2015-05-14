(ns chapter3.part1.assignment-and-state
  (:require [chapter3.part1.assignment-and-state-samples :as samples]))

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

;; 3.3
(defn make-account-password [balance password]
  (let [balance (atom balance)
        withdraw (fn [amount]
                   (if (>= @balance amount)
                     (swap! balance - amount)
                     "Insufficient funds"))
        deposit (fn [amount]
                  (swap! balance + amount))
        dispatch (fn [m]
                   (cond (= m :withdraw) withdraw
                         (= m :deposit) deposit
                         :else (samples/error "Unknown request: MAKE-ACCOUNT" m)))]
    (fn [pass-req operation]
      (if (not= password pass-req)
        (samples/error "Wrong password")
        (dispatch operation)))))

;; 3.4
(defn call-the-cops [_])
(defn make-account-password-cops [balance password]
  (let [attemps (atom 0)
        A (make-account-password balance password)]
    ;; execute action on A
    ;; catch Exception
    ;; increase attemps
    ;; if = 7, call call-the-cops
    ;; rethrow Exception))
