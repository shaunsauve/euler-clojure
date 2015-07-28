;
; Smallest multiple
; Problem 5
; 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
;
; What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

(defn factor-of? [p q] (= 0 (mod p q)))

(reduce * (range 1 21))
;=> 2432902008176640000

;; - multiple all of the numbers from 1 to 20 to create a starting point
;; - then start dividing  a * b * c'

; n = pa + qb + rc
; m = za + yb + zc


(defn remove-redundant-factors [coll]
  (loop [[next & remaining] coll
         new-coll #{}]
    (if (nil? next)
      new-coll
      (recur remaining
             (if (some #(and (not= next %) (factor-of? % next)) coll)
               new-coll
               (conj new-coll next))))))

(defn smallest-product-of [coll]
  (let [reduced-coll (remove-redundant-factors coll)]
    (loop [iteration 1]
      (if (every? #(factor-of? iteration %) reduced-coll)
        iteration
        (recur (inc iteration))))))

(smallest-product-of (range 1 11))
;=> 2520
(smallest-product-of (range 1 21))
;=> 232792560

; realization #1: for N to be smallest multiple, it's prime factorization must include the
; smallest number of prime factors.  Prime-factorization is factoring it down to the product of just
; prime numbers.
;
; 2 * 3 * 2 * 5 * 7 * 2 * 3 * 11 * 13 * 2 * 17 * 19 = 232792560
;
; more..