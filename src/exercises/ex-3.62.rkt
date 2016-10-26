#lang racket
(require "../examples/streams.rkt")
(require "ex-3.59.rkt")
(require "ex-3.60.rkt")
(require "ex-3.61.rkt")

;; Exercise 3.62: Use the results of Exercise 3-60 and Exercise 3-61
;; to define a procedure `div-series' that divides two power series.
;; `Div-series' should work for any two series, provided that the
;; denominator series begins with a nonzero constant term.  (If the
;; denominator has a zero constant term, then `div-series' should
;; signal an error.)

(define (div-series n d)
  (if (= (stream-car d) 0)
      (error "DIV-SERIES -- Denominator starts with 0")
      (mul-series n (invert-unit-series d))))

;; Show how to use `div-series' together with the result of Exercise
;; 3-59 to generate the power series for tangent.

(define tangent-series
  (div-series sine-series cosine-series))
