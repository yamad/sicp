#lang racket
(require "../examples/streams.rkt")
(require "ex-3.74.rkt")
(require "ex-3.50.rkt")

;; Exercise 3.76: Eva Lu Ator has a criticism of Louis's approach in
;; Exercise 3-75. The program he wrote is not modular, because it
;; intermixes the operation of smoothing with the zero-crossing
;; extraction.  For example, the extractor should not have to be
;; changed if Alyssa finds a better way to condition her input signal.
;; Help Louis by writing a procedure `smooth' that takes a stream as
;; input and produces a stream in which each element is the average of
;; two successive input stream elements.  Then use `smooth' as a
;; component to implement the zero-crossing detector in a more modular
;; style.

(define (average . xs)
  (/ (apply + xs) (length xs)))

(define (smooth stream)
  (stream-map-multiple average
                       stream (stream-cdr stream)))

(define (zero-crossings-conditioned input-stream conditoner)
  (define conditioned (conditoner input-stream))
  (define go
    (stream-map-multiple sign-change-detector
                         conditioned (stream-cdr conditioned)))
  go)
