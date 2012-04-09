#lang racket

(define (square x) (* x x))

; exercise 1.28
; Miller-Rabin primality test
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

(define (mr-prime? n)
  (fast-prime? n 10))

(require rackunit)
(check-eq? (mr-prime? 1) false)
(check-eq? (mr-prime? 4) false)
(check-eq? (mr-prime? 7) true)

;; Carmichael numbers are not prime, but fool the Fermat test
(check-eq? (mr-prime? 561) false)
(check-eq? (mr-prime? 1105) false)
(check-eq? (mr-prime? 1729) false)
(check-eq? (mr-prime? 2465) false)
(check-eq? (mr-prime? 6601) false)
(check-eq? (mr-prime? 6603) false)