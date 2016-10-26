#lang racket
(require "../examples/streams.rkt")
(require "ex-3.55.rkt")                 ; partial-sums
(require "ex-3.64.rkt")                 ; stream-limit

;; Exercise 3.65: Use the series
;;
;;               1   1   1
;;    ln 2 = 1 − - + − - - + ...
;;               2   3   4
;;
;; to compute three sequences of approximations to the natural
;; logarithm of 2, in the same way we did above for pi. How rapidly do
;; these sequences converge?

(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2-summands (+ n 1)))))

(define ln2-stream
  (partial-sums (ln2-summands 1)))

(define ln2-stream-euler
  (euler-transform ln2-stream))

(define ln2-stream-tableau
  (accelerated-sequence euler-transform ln2-stream))


;; with no acceleration, can't converge to 4 decimal places by 5000 elements
;; with euler transform,       converges to 6 decimal places within 100 elements
;; with tableau acceleration,  converges to 7 decimal places in 3 elements
;;                             converges to 9 decimal places in 5 elements
