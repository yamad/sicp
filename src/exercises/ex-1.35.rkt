#lang racket

;; exercise 1.35
;; show that phi (the golden ratio) is a fixed point of the
;; transformation x --> 1 + 1/x

;; A fixed point is defined as an x in the domain of f where f(x) = x
;; The golden ratio satisfies the equation phi^2 = phi + 1

;; transforming the function:
;; x --> 1 + 1/x
;; x * x --> x (1 + 1/x)
;; x^2 --> x + 1

;; by the definition of a fixed point, then we need an x that
;; satisfies x^2 = x + 1. This x is precisely the golden ratio, by the
;; definition of the golden ratio.


;; fixed-point approximation procedure (pg. 69)
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))


;; approximate the golden ratio
(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)