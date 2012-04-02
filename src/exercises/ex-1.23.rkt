#lang racket

;; Naive O(sqrt(n)) algorithm for finding primes (provided code)
(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

;; exercise 1.23, skips even numbers
(define (next n)
  (if (= n 2) 3
      (+ n 2)))

(define (square x) (* x x))

(define (divides? a b)
  (= (remainder b a) 0))

;; Prime test timer (provided code)
(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime) start-time))
      #f))

(define (report-prime n elapsed-time)
  (display n)
  (display " *** ")
  (display elapsed-time)
  (newline)
  #t)

(define (runtime)
  (current-inexact-milliseconds))

;; exercise 1.23
(define (search-for-primes min amt)
  (define (print-n-primes min count)
    (cond ((= count 0) (void))
          ((timed-prime-test min) (print-n-primes (+ 2 min) (- count 1)))
          (else (print-n-primes (+ 2 min) count))))
  (print-n-primes (if (even? min)
                      (+ min 1)
                      min)
                  amt))

(search-for-primes 1000000000 5)
(search-for-primes 10000000000 5)
(search-for-primes 100000000000 8)