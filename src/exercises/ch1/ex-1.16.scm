#!/usr/bin/env mzscheme
#lang scheme

;; ex 1.16
;; -------

;; An iterative, log growth, successive square process for calculating
;; the b to the power of n

(define (square x) (* x x))
(define (even? x) (= (remainder x 2) 0))

;; My version
(define (fast-expt-iter b n)
  (define (iter b n a)
    (if (= n 0)
        a
        (iter b (- n 1) (* a (square b)))))          
  (if (even? n)
      (iter b (/ n 2) 1)
      (iter b (/ (- n 1) 2) b)))

;; Scheme Wiki version
(define (fast-expt-iter-other b n)
  (define (iter b n a)
    (cond ((= n 0) a)
          ((even? n) (iter (square b) (/ n 2) a))
          (else (iter b (- n 1) (* a b)))))
  (iter b n 1))

(require (planet schematics/schemeunit:3))
(require (planet schematics/schemeunit:3/text-ui))

(define tests
  (test-suite
   "Tests"
   (check-eq? (fast-expt-iter 2 10)
              1024)
   (check-eq? (fast-expt-iter 4 1)
              4)
   (check-eq? (fast-expt-iter 3 7)
              2187)
   (check-eq? (fast-expt-iter 5 0)
              1)
   (check-eq? (fast-expt-iter-other 2 10)
              1024)
   ))
(run-tests tests)