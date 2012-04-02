#lang racket

;; Fermat's Little Theorem
;; probablistic algorithm for finding primes
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

; base^exp modulo m
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (square x) (* x x))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

;; Prime test timer (provided code)
(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 10)
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

(search-for-primes 10000 3)
(search-for-primes 100000000 3)
