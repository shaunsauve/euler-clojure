; Sum square difference
; Problem 6
; The sum of the squares of the first ten natural numbers is,
; 1^2 + 2^2 + ... + 10^2 = 385
;
; The square of the sum of the first ten natural numbers is,
; (1 + 2 + ... + 10)^2 = 552 = 3025
;
; Hence the difference between the sum of the squares of the first ten natural numbers and the
; square of the sum is 3025 − 385 = 2640.
;
; Find the difference between the sum of the squares of the first one hundred natural numbers and
; the square of the sum.

(defn sum-of-squares [coll]
  (reduce + (map #(* % %) coll)))

(sum-of-squares (range 1 11))
;=> 385

(defn square-of-sums [coll]
    (#(* % %) (reduce + coll)))

(square-of-sums (range 1 11))
;=> 3025

(let [coll (range 1 101)]
  (- (square-of-sums coll) (sum-of-squares coll)))
;=> 25164150

(loop [iteration 1]
  (if (< iteration 15)
    (let [coll (range 1 (inc iteration))
          sum (sum-of-squares coll)
          square (square-of-sums coll)
          diff (- square sum)]
      (do
        (println iteration ":" square "-" sum "=" diff)
        (recur (inc iteration))))))

; 1 : 1 - 1 = 0
; 2 : 9 - 5 = 4
; 3 : 36 - 14 = 22
; 4 : 100 - 30 = 70
; 5 : 225 - 55 = 170
; 6 : 441 - 91 = 350
; 7 : 784 - 140 = 644
; 8 : 1296 - 204 = 1092
; 9 : 2025 - 285 = 1740
; 10 : 3025 - 385 = 2640
; 11 : 4356 - 506 = 3850
; 12 : 6084 - 650 = 5434
; 13 : 8281 - 819 = 7462
; 14 : 11025 - 1015 = 10010

; = (1 + 2 + .. + n)^2 - (1^2 + 2^2 + .. + n^2)
; = (1 + 2 + .. + n)(1 + 2 + .. + n-1 + n) - (1^2 + 2^2 + .. + n-1^2 + n^2)
; = (1*1 + 1*2 + .. + 1*n) + (2*1 + 2*2 + .. + 2*n) + .. + (1*n + 2*n + .. + n-1*n + n*n) - (1*1 + 2*2 + .. + n-1*n-1 + n*n)
; = (xxx + 1*2 + .. + 1*n) + (2*1 + xxx + .. + 2*n) + .. + (1*n + 2*n + .. + n-1*n + xxx) - (xxx + xxx + .. + xxxxxxx + xxx)
; what am i doing again.. ?

; realization #1: sum(n) = n(n + 1)/2 which is widely known. Consider the range from 1 to 100.
;   If you split the series evenly and starting from both ends you pairs elements from the first
;   half with elements from the second you get:
;   (1, 100), (2, 99), (3, 98), (4, 97) ... (50, 51) where the sum of any pair is equal to 101.
;   Generally we recognize that there are n/2 pairs, and each of them sums to n + 1.  So
;   (n + 1) * n/2 = sum of all pairs sums = sum of all numbers in the range

(#(let [sum (/ (* % (+ % 1)) 2)]
   (* sum sum)) 100)
;=> 25502500

(defn square [n] (* n n))

(#(->
   (+ % 1)
   (* %)
   (/ 2)
   (square)) 100)
;=> 25502500

