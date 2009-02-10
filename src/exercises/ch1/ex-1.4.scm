#! /usr/bin/env mzscheme
#lang scheme

;; ex 1.4
;; ------
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
