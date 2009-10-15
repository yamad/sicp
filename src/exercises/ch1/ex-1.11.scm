#!/usr/bin/env mzscheme
#lang scheme

;; Two versions of the function `f`, see the recursive procedure for its definition

;; Recursive
(define (f-rec n)
  (if (< n 3)
      n
      (+ (f-rec (- n 1))
         (* 2 (f-rec (- n 2)))
         (* 3 (f-rec (- n 3))))))

;; Iterative
(define (f-iter n)
  (iter 2 1 0 n))
(define (iter a b c count)
  (if (< count 3)
      a
      (iter (+ a (* 2 b) (* 3 c))
            a
            b
            (- count 1))))

;; (f-rec 4)
;; (+ (f-rec 3) (* 2 (f-rec 2)) (* 3 (f-rec 1)))
;; (+ (f-rec 3) (* 2 2) (* 3 1))
;; (+ (f-rec 3) 4 3)
;; (+ (+ (f-rec 2) (* 2 (f-rec 1)) (* 3 (f-rec 0))) 4 3)
;; (+ (+ 2 (* 2 1) (* 3 0)) 4 3)
;; (+ (+ 2 2 0) 4 3)
;; (+ 4 4 3)
;; 11

;; (f-iter 4)
;; (iter (+ 2 (* 2 1) (* 3 0)) 2 1 3)
;; (iter 4 2 1 3)
;; (iter (+ 4 (* 2 2) (* 3 1)) 4 2 2)
;; (iter 11 4 2 2)
;; 11


(require (planet schematics/schemeunit:3))
(require (planet schematics/schemeunit:3/text-ui))

(define tests
  (test-suite
   "Tests"
   (check-eq? (f-rec 2)
              (f-iter 2))
   (check-eq? (f-rec 3)
              (f-iter 3))
   (check-eq? (f-rec 5)
              (f-iter 5))
   ))
(run-tests tests)