#lang racket/base

;; exercise 1.18

;; assume double and halve are language built-ins
(define (double a) (+ a a))
(define (halve a) (/ a 2))

;; Return the product of positive integers a and b. O(log n) steps, iterative
(define (fast-mult-iter a b)
  (define (iter a b c)
    (cond ((= b 0) c)
          ((even? b) (iter (double a) (halve b) c))
          (else (iter a (- b 1) (+ c a)))))
  (iter a b 0))

(require rackunit)
(check-eq? (fast-mult-iter 2 0) 0)
(check-eq? (fast-mult-iter 2 4) 8)
(check-eq? (fast-mult-iter 2 5) 10)