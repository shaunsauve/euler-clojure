; Largest palindrome product
; Problem 4
; A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers
; is 9009 = 91 × 99.
;
; Find the largest palindrome made from the product of two 3-digit numbers.


(defn smallest-n-digit-number [digits] (int (Math/pow 10 (- digits 1))))
(defn largest-n-digit-number [digits] (- (int (Math/pow 10 digits)) 1))
(= 10000 (int (Math/pow (smallest-n-digit-number 3) 2)))
(= 998001 (int (Math/pow (largest-n-digit-number 3) 2)))

