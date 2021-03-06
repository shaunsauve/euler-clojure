; Even Fibonacci numbers
; Problem 2
;
; Each new term in the Fibonacci sequence is generated by adding the previous two terms. By starting with 1 and 2, the
; first 10 terms will be:
; 
; 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
; 
; By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the
; even-valued terms.


(defn sum-of-even-fibs
  [limit]
  (loop [n1 0
         n2 1
         sum 0]
      (let [next-term (+ n1 n2)]
        (println next-term)
        (if (< limit next-term)
          sum
          (if (= 0 (mod next-term 2))
            (recur n2 next-term (+ sum next-term))
            (recur n2 next-term sum))))))

; turns out that every third fib is even, and upon examining them
; 2 8 34 144 ..
; it turns out they also follow E(n) = 4*E(n-1) + E(n-2)
(defn better-sum-of-even-fibs
  [limit]
  (loop [n1 0
         n2 2
         sum 0]
    (let [next-term (+ n1 (* 4 n2))]
      (if (< limit next-term)
        sum
        (recur n2 next-term (+ sum next-term))))))


(sum-of-even-fibs 4000000)
