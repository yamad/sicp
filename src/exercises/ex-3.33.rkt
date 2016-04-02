#lang racket

(require "../examples/constraints.rkt")

;; c is the average of a and b
(define (averager a b c)
  (let ((x (make-connector))
        (half (make-connector)))
    (adder a b x)
    (multiplier x half c)
    (constant 1/2 half))
  'ok)

;; test
(define a (make-connector))
(define b (make-connector))
(define c (make-connector))
(averager a b c)
(probe "Value of a" a)
(probe "Value of b" b)
(probe "Average of a and b" c)
(set-value! a 3 'user)
(set-value! b 7 'user)

(forget-value! a 'user)
(set-value! c 5 'user)

(forget-value! b 'user)
