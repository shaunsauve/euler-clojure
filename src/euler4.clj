; Largest palindrome product
; Problem 4
; A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers
; is 9009 = 91 × 99.
;
; Find the largest palindrome made from the product of two 3-digit numbers.

(use 'criterium.core)

(require 'clojure.tools.trace)

(defn smallest-n-digit-number [digits] (int (Math/pow 10 (- digits 1))))
(defn largest-n-digit-number [digits] (- (int (Math/pow 10 digits)) 1))
(defn square [n] (* n n))

(= 10000 (square (smallest-n-digit-number 3)))
(= 998001 (square (largest-n-digit-number 3)))
;; all palindromes in descending order 997799 996699 995599 994499 993399 992299 991199 990099 989989 ..

(defn get-digit
  "return the digit at position n where 0 is the right-most digit of the number"
  [n index]
  (int (mod (/ n (Math/pow 10 index)) 10)))

(defn digit-count
  "The logarithm of a number is the exponent to which another fixed value, the
  base, must be raised to produce that number. In the reverse operation we
  raise base 10 to a power, the number of digits of the result is the exponent - 1"
  [n]
  (-> n Math/log10 Math/floor long inc))

; test
(let [n (rand-int 9999999)] (conj [n] (map #(get-digit n %) (range 0 (digit-count n)))))
; => [9542394 (4 9 3 2 4 5 9)]

(defn is-palindrome? [n]
  (loop
    [head 0, tail (dec (digit-count n))]
    (if (>= head tail)
      true
      (if (= (get-digit n head) (get-digit n tail))
        (recur (inc head) (dec tail))
        false))))

(defn reverse-number [n]
  ;12345
  ;1234 => 5
  ;123 => 4 + 50
  ;12 => 3 + 40 + 500
  ;1 => 2 30 400 5000
  ;1 + 20 + 300 + 4000 + 50000

  (loop [reverse 0
         remaining n]
    (if (= remaining 0)
      reverse
      (recur (+ (* reverse 10) (mod remaining 10)) (bigint (/ remaining 10)) ))))

(defn is-palindrome-v2? [n]
  "instead of isolating digits and iterating from both ends we can generate the reverse of the
  number and compare it to itself"
  (= n (reverse-number n)))

; the original appears to be almost twice as fast (profiled with criterium/time-body). in v2 and v1 are both
; O(n). In v2 reverse-number does all the work and uses 4 arithmetic operations and a cast to int (ignore the
; implications of bigint for now, the results were approximately the same with just int). In v1 we have 3
; arithmetic functions in get-digit (including Math/pow that problably reduces to 2^(y*log2(x))), and 2 more
; increments in the top function.  So at first glance I would have thought the the second might be faster..


(defn factor-of? [p q] (= 0 (mod p q)))
(defn between? [n p q] (and (> p n) (> n q)))

;; handy macro for debugging parts of expressions
(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(defn brute-force-has-factors-in-between?
  [n p q]
  (let [upper-bound (max p q)
        lower-bound (min p q)]
    (loop [next upper-bound]
      (if (<= next lower-bound)
        false
        (if (and (factor-of? n next)
                 (between? (/ n next) (inc upper-bound) (dec lower-bound)))
          true
          (recur (dec next)))))))

; oops, this can be done with (some #(and (..) %) []), but this is more legible anyway
(defn find-first [pred coll]
  (loop [[next & remaining] coll]
    (if (nil? next) nil (if (pred next) next (recur remaining)))))

(let [upper-bound (largest-n-digit-number 3)
      lower-bound (smallest-n-digit-number 3)]
  (find-first
    (fn [n] (and (is-palindrome? n) (brute-force-has-factors-in-between? n upper-bound lower-bound)))
    ; the range is in descending order, so the first palindrome that satifies our predicate will be largest
    (range (square upper-bound) (square lower-bound) -1)))
;=> 906609

;; ways to improve: try enumerating by number