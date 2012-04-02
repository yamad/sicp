#lang racket/base
; searching for divisor, pg. 50

(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(display (smallest-divisor 199)) (display "\n")
(display (smallest-divisor 1999)) (display "\n")
(display (smallest-divisor 19999)) (display "\n")