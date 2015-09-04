; Multiples of 3 and 5
; Problem 1
; If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these
; multiples is 23.
; 
; Find the sum of all the multiples of 3 or 5 below 1000.

(defn factor-of-all?
  [factors number]
  (if (empty? factors)
    true
    (let [next-factor (first factors)]
      (if (= 0 (mod number next-factor))
        (factor-of-all? (rest factors) number)
        false))))

(defn factor-of-any?
  [factors number]
  (if (empty? factors)
    false
    (let [next-factor (first factors)]
      (if (= 0 (mod number next-factor))
        true
        (factor-of-any? (rest factors) number)))))

(defn sum-of-factors
  [factors limit]
  (loop [iteration 0
         sum 0]
    (if (= iteration limit)
      sum
      (if (factor-of-any? factors iteration)
        (recur (inc iteration) (+ sum iteration))
        (recur (inc iteration) sum)))))


(sum-of-factors [3 5] 1000)
;=> 233168