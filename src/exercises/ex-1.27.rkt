#lang racket

(define (square x) (* x x))

; Fermat's Little Theorem, without random numbers
(define (fermat-little a n)
  (= (expmod a n n) a))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

;; exercise 1.27
;; apply fermat test to *all* numbers below n
(define (deep-prime? n)
  (define (iter a n)
    (cond ((= a n) true)
          ((fermat-little a n) (iter (+ a 1) n))
          (else false)))
  (if (<= n 1) false
      (iter 2 n)))

(require rackunit)
(check-eq? (deep-prime? 1) false)
(check-eq? (deep-prime? 4) false)
(check-eq? (deep-prime? 7) true)

;; Carmichael numbers are not prime, but fool the Fermat test
(check-eq? (deep-prime? 561) true)
(check-eq? (deep-prime? 1105) true)
(check-eq? (deep-prime? 1729) true)
(check-eq? (deep-prime? 2465) true)
(check-eq? (deep-prime? 6601) true)
(check-eq? (deep-prime? 6603) true)