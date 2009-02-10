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
