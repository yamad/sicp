#lang racket

(define (cons-a x y)
  (lambda (m) (m x y)))

(define (car-a z)
  (z (lambda (p q) p)))

(define (cdr-a z)
  (z (lambda (p q) q)))

(car-a (cons-a 1 2))
(cdr-a (cons-a 1 2))
