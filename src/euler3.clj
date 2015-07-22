; Largest prime factor
; Problem 3
; The prime factors of 13195 are 5, 7, 13 and 29.
;
; What is the largest prime factor of the number 600851475143 ?

;; prime factor is a prime number (whole number that is divisible only by itself and 1) that is also a factor
;; (positive integer that divides the number exactly)

(use 'criterium.core)

(defn factor-of? [p q] (= 0 (mod p q)))

(defn naive-is-prime?
  "naive implementation without consulting the interwebs"
  [number]
  (if (< number 2)
    false
    (loop [iteration (dec number)]
      (if (= iteration 1)
        true
        (if (factor-of? number iteration)
          false
          (recur (dec iteration)))))))

; for testing
(defn primes-under [under-number prime-test?]
  (loop [i 0
         primes []]
    (if (< i under-number)
      (if (prime-test? i)
        (recur (inc i) (conj primes i))
        (recur (inc i) primes))
      primes)))

(def primes-under-100 [2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97])
(= primes-under-100 (primes-under 100 naive-is-prime?))

(defn naive-largest-prime-factor [number prime-test?]
  (if (<= number 1)
    nil
    (loop [iteration (dec number)]
      (if (and
            (factor-of? number iteration)
            (prime-test? iteration))
        iteration
        (recur (dec iteration))))))

(= 29 (naive-largest-prime-factor 13195 naive-is-prime?))

; for exploration
(defn not-divisible-by-under [factors under-number]
  (loop [[next-factor & remaining-factors] factors
         results (range 0 under-number)]
    (if (nil? next-factor)
      results
      (recur remaining-factors (filter #(not (factor-of? % next-factor)) results)))))

(def all-numbers-not-divisible-by-2-or-3-under-100 [1 5 7 11 13 17 19 23 25 29 31 35 37 41 43 47 49 53 55 59 61 65 67 71 73 77 79 83 85 89 91 95 97])
(= all-numbers-not-divisible-by-2-or-3-under-100 (not-divisible-by-under [2 3] 100))

"intertubes to the rescue:
  i. http://stackoverflow.com/questions/9625663/calculating-and-printing-the-nth-prime-number"

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
              (recur (+ next-factor-to-try (get pattern (mod step pattern-size))) (inc step))
              )
            )
          )
        )
      )
    )
  )


(= primes-under-100 (primes-under 100 trial-division-is-prime?))
(= 29 (naive-largest-prime-factor 13195 trial-division-is-prime?))

(defn slightly-smarter-largest-prime-factor
  "growing upward, we limit the range using same tricks learned for testing prime"
  [number prime-test?]
  (if (< number 7)
    (if (or (= number 2) (= number 3) (= number 5))
      number
      nil)
    (let [pattern [4 2 4 2 4 6 2 6]                         ; see realization #4
          pattern-size (count pattern)
          limit (int (Math/sqrt number))]                   ; see realization #2
      (loop [next-factor-to-try 7
             step 0
             largest nil]
        (if (> next-factor-to-try limit)                    ; see realization #3
          largest
          (if (and (factor-of? number next-factor-to-try)
                   (prime-test? next-factor-to-try))
            (recur (+ next-factor-to-try (get pattern (mod step pattern-size))) (inc step) next-factor-to-try)
            (recur (+ next-factor-to-try (get pattern (mod step pattern-size))) (inc step) largest)
            )
          )
        )
      )
    )
  )

"next steps if we want to squeeze out more performance:
 i.  avoid multiples of more prime numbers by iterating over a more complicated pattern. Skipping subsequently
     more multiples of prime would reduce the number of divisions necessary to 48/210 (7), 480/2310 (11),
     5760/30030 (13) ...
 ii. apply some of the fast primality tests using number-theory properties"

(= 6857 (slightly-smarter-largest-prime-factor 600851475143 trial-division-is-prime?))

(defn ns-to-seconds [ns] (float (/ ns 1e9)))

(ns-to-seconds (first (time-body (naive-largest-prime-factor 123456789 trial-division-is-prime?))))
; => 7.400868
(ns-to-seconds (first (time-body (slightly-smarter-largest-prime-factor 123456789 trial-division-is-prime?))))
; => 0.00123318


; spoiler
(defn prime? [n]
  (if (= n 2) n
              (if (even? n)
                nil
                (loop [i 3]
                  (if (>= (. Math sqrt n)) n
                                           (if (= (rem n i) 0)
                                             nil
                                             (recur (inc (inc i)))))))))

(defn largest-prime-factor [n]
  (loop [i 2 left n]
    (if (= left 1) (dec i)
                   (recur (inc i)
                          (if (and (prime? i) (= (rem n i) 0))
                            (loop [div left]
                              (if (= (rem div i) 0) (recur (/ div i))
                                                    div))
                            left)))))

(println (largest-prime-factor 600851475143))