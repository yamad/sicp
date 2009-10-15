#!/usr/bin/env mzscheme
#lang scheme

;; ex 1.17
;; -------

;; An iterative, log growth, successive addition process for doing
;; multiplication

(define (even? x) (= (remainder x 2) 0))
(define (double x) (+ x x))
(define (halve x) (/ x 2.0))

(define (fast-mult x y)
  (define (iter x y prod)
    (cond ((= y 0) prod)
          ((even? y) (iter (double x) (halve y) prod))
          (else (iter x (- y 1) (+ x prod)))))
  (iter x y 0))


(require (planet schematics/schemeunit:3))
(require (planet schematics/schemeunit:3/text-ui))

(define tests
  (test-suite
   "Tests"
   (check-eq? (fast-mult 5 10)
              50)
   (check-eq? (fast-mult 6 7)
              42)
   ))
(run-tests tests)