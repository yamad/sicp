#! /usr/bin/env mzscheme
#lang scheme

;; ex 1.3
;; ------
;; a function that takes 3 arguments and finds the 
;; sum of squares for the two largest numbers

(define (square x) 
  (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))


(define (sum-of-squares-largest-two x y z)
  (if (>= x y)
      (if (>= z y) (sum-of-squares x z) (sum-of-squares x y))
      (if (>= z x) (sum-of-squares z y) (sum-of-squares x y))))


;; tests
(sum-of-squares-largest-two 1 2 3)
(sum-of-squares-largest-two 2 1 3)
(sum-of-squares-largest-two 3 1 2)
(sum-of-squares-largest-two 3 2 1)
;; result: 13
