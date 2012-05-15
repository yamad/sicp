#lang racket

;; exercise 1.33 - filtered accumulate
(define (filtered-accumulate filter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (if (filter a)
                                     (term a)
                                     null-value)
                                 result))))
  (iter a null-value))

;; recursive version (just for fun)
(define (filtered-accumulate-rec filter combiner null-value term a next b)
  (if (> a b)
      null-value
      (if (filter a)
          (combiner (term a)
                    (filtered-accumulate-rec filter combiner null-value term (next a) next b))
          (filtered-accumulate-rec filter combiner null-value term (next a) next b))))



;; a) sum of squares of primes in interval
;; ==============================================

;; use Miller-Rabin prime test from exercise 1.28
(define (square x) (* x x))
(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod-check a (- n 1) n) 1))
  (if (<= n 1) false
      (try-it (+ 1 (random (- n 1))))))
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (else false)))
(define (expmod-check base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (check-nontrivial-sqrt (expmod-check base (/ exp 2) m) m))
        (else
         (remainder (* base (expmod-check base (- exp 1) m)) m))))
(define (check-nontrivial-sqrt x m)
  (if (and (not (or (= x 1) (= x (- m 1))))
           (= (remainder (square x) m) 1))
      0
      (remainder (square x) m)))
(define (prime? n)
  (fast-prime? n 20))
(define (inc x) (+ x 1))


;; sum of squares of prime numbers in interval a to b
(define (sum-squared-primes a b)
  (filtered-accumulate prime? + 0 square a inc b))

(sum-squared-primes 1 3)                ; --> 13
(sum-squared-primes 1 6)                ; --> 38
(sum-squared-primes 1 10)               ; --> 87



;; b) product of all coprimes of n from 1 to n
;; ===========================================

;; greatest common divisor from page 49
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))


(define (product-of-coprimes n)
  (define (coprime-to-n? k)
    (= 1 (gcd k n)))
  (filtered-accumulate coprime-to-n? * 1 identity 1 inc (- n 1)))

(product-of-coprimes 10)                 ; --> 189
(product-of-coprimes 11)                 ; --> 3628800
