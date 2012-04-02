#lang racket/base

;; exercise 1.17

;; assume double and halve are language built-ins
(define (double a) (+ a a))
(define (halve a) (/ a 2))

;; Return the product of positive integers a and b. O(log n) steps, recursive
(define (fast-mult a b)
  (cond ((= b 0) 0)
        ((even? b) (double (fast-mult a (halve b))))
        (else (+ a (fast-mult a (- b 1))))))

(require rackunit)
(check-eq? (fast-mult 2 0) 0)
(check-eq? (fast-mult 2 4) 8)
(check-eq? (fast-mult 2 5) 10)
