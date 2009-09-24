#! /usr/bin/env mzscheme
#lang scheme

;; ex 1.7
;; ------

;; The initial sqrt program

(define (square x) (* x x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;; test `sqrt` on small numbers.
(sqrt 0.0002)
;; result should be: 0.01414
(sqrt 0.0001)
;; result shoud be: 0.01

;; this doesn't work because 0.001 is significant when comparing the
;; square of a very small number.

;; test `sqrt` on large numbers
;(sqrt 998899889988998899889988)
;; result should be: 9.99e11

;; this will never terminate b/c the machine cannot represent
;; differences small enough for the number to change


;; New good-enough?

;; operates relative to guess itself so magnitude is correct
;; difference between guess must be a small fraction of the guess
(define (my-good-enough? guess last-guess)
  (< (abs (- guess last-guess)) (* guess 0.001)))

;; signature of good-enough has changed, so sqrt-iter has to change
(define (my-sqrt-iter guess last-guess x)
  (if (my-good-enough? guess last-guess)
      guess
      (my-sqrt-iter (improve guess x)
                 guess
                 x)))

;; initial guesses must be off from each other to force at least one
;; iteration
(define (my-sqrt x)
  (my-sqrt-iter 1.0 0.0 x))

;; test `sqrt` on small numbers.
(my-sqrt 0.0002)
;; result should be: 0.01414
(my-sqrt 0.0001)
;; result shoud be: 0.01
