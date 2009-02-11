#! /usr/bin/env mzscheme
#lang scheme

;; ex 1.5
;; ------
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

;: (test 0 (p))
   
;; applicative order -- evaluate subexpressions, then apply operand
(test 0 (p))
(test 0 (p))
; interpreter in infinite loop as it "expands" (p) to itself repeatedly

;; normal order -- expand fully, then reduce (step by step)
(test 0 (p))
(if (= 0 0)
   0
   (p))
(if #t
   0
   (p))
; result: 0
