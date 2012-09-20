#lang racket

;; ex 1.46
(define (iterative-improve good-guess improve)
  (lambda (a)
    (let ((a-im (improve a)))
      (if (good-guess a a-im)
          a-im
          ((iterative-improve good-guess improve) a-im)))))

(define tolerance 0.0001)
(define (good-enough? a b)
  (< (abs (- a b)) tolerance))

(define (fixed-point f first-guess)
  ((iterative-improve good-enough? f) first-guess))

(define (sqrt x)
  ((iterative-improve
    good-enough?
    (lambda (y)
      (/ (+ (/ x y) y) 2)))
   1.0))

(sqrt 2)
