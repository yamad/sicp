#lang racket/base

(define (square x) (* x x))

;; exercise 1.16
;; Return base b raised to the nth power. O(log n) iterative algorithm.
;;
;; Notes: uses successive squaring: b^n = (b^2)^(n/2)
;; formula a(b^n) is always equal to the final result
;;
;; a is used to store result with odd-numbered exponents, which means
;; it will always be used in the final step when n=1.
(define (fast-expt-iter b n)
  (define (iter b n a)
    (cond ((= n 0) a)
          ((even? n) (iter (square b) (/ n 2) a))
          (else (iter b (- n 1) (* a b)))))
  (iter b n 1))

;; tests
(require rackunit)
(check-eq? (fast-expt-iter 2 0) 1)
(check-eq? (fast-expt-iter 2 1) 2)
(check-eq? (fast-expt-iter 2 2) 4)
(check-eq? (fast-expt-iter 2 3) 8)
(check-eq? (fast-expt-iter 0 1) 0)