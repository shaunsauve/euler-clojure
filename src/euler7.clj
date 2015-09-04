; 10001st prime
; Problem 7
; By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is
; 13.
;
; What is the 10001st prime number?

;;; from euler3
(defn factor-of? [p q] (= 0 (mod p q)))

(defn trial-division-is-prime?
  "more efficient brute force method

  realization 1: if n is composite (not prime) anything that can cleanly divide n must be < n / 2
  realization 2: along the same lines as #1, divisors of n come in pairs (d, n/d)
    and we will catch the smaller of the two first as we iterate from 0, and
    need not continue scanning to find the larger. Since the inflection point
    where we transition from discovery of smaller divisors to larger is when d =
    n/d, sqrt(n). So we need not scan past that if we're just trying to determine if
    at least one set of factors exist other then 1 and itself
  realization 3: no prime other then 2 is even and no prime other then 3 is divisible by 3.  If you consider
    all numbers NOT in these two families they are 5 7 11 13 17 19 23 25 29 31... which exhibits an easy to follow
    alternating pattern of n = (n-1) + 2, n = (n-1) + 4 (or to put it another way, for k > 1, all 6k +- 1)
  realization 4: you can keep going like this eliminating all multiples of subsequent primes 5, 7, 11... but
    code complexity increases and there are diminishing returns.  For example, numbers that are not factors of
    2, 3, 5 look like 7 11 13 17 19 23 29 31 37 | 41 43 47 49 53 59 61 67 | 71 73 77 79 83 89 91 97 ... which follows
    a more complicated pattern starting with 7 of + [ 4 2 4 2 4 6 2 6 | 4 2 4 2 4 6 2 6 | 4 2 4 2 4 6 2 6 ... ]
  "
  [number]
  (if (< number 2)
    false
    (if (or (factor-of? number 2) (factor-of? number 3) (factor-of? number 5))
      (or (= number 2) (= number 3) (= number 5))           ; the only primes divisible by 2,3,5 are 2,3 or 5!
      (let [pattern [4 2 4 2 4 6 2 6]                       ; see realization #4
            pattern-size (count pattern)
            limit (int (Math/sqrt number))]                 ; see realization #2
        (loop [next-factor-to-try 7
               step 0]
          (if (> next-factor-to-try limit)                  ; see realization #3
            true
            (if (factor-of? number next-factor-to-try)
              false
              (recur (+ next-factor-to-try (get pattern (mod step pattern-size))) (inc step)))))))))

(defn nth-prime [n coll]
  (let [next (first coll)
        remainder (rest coll)]
    (if next
      (if (trial-division-is-prime? next)
        (if (= 1 n) next (recur (dec n) remainder))
        (recur n remainder)))))

(nth-prime 10001 (range 1 1000000))
;;;=> 104743