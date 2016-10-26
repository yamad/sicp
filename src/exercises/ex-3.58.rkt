#lang racket
(require "../examples/streams.rkt")

;; Exercise 3.58: Give an interpretation of the stream computed by the
;; following procedure:

(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

;; (Quotient is a primitive that returns the integer quotient of two
;; integers.) What are the successive elements produced by `(expand
;; 1 7 10)`? What is produced by `(expand 3 8 10)`?

;; ans: `expand` returns the decimal expansion of a fraction with
;; numerator `num` and denominator `den` in the `radix` base number
;; system.
;;
;; e.g.

;; 1/7 --> 0.142857142...
;; (expand 1 7 10) --> 1 4 2 8 5 7 1 4 2 ...

;; 3/8 --> 0.375
;; (expand 3 8 10) --> 3 7 5 0 0 ...
