#! /usr/bin/env mzscheme
#lang scheme

;; ex 1.6
;; ------

;; Define an `if` function as an ordinary procedure using `cond`
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

;; test `new-if`
(new-if (> 2 1) (display 'True) (display 'False))
;; result: TrueFalse

;; note that both clauses are evaluated using the applicative-order
;; eval model of the non-special procedure `new-if`

;; test `if`
(if (> 2 1) (display 'True) (display 'False))
;; result: True

;; note that False does not display in this case because it is not
;; evaluated under the `if` special form normal-order evaluation model

