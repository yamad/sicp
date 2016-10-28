#lang racket
(require "../examples/streams.rkt")
(require "ex-3.66.rkt")

;; Exercise 3.69: Write a procedure triples that takes three infinite
;; streams, S, T, and U, and produces the stream of triples (S_i , T_j
;; , U_k) such that i ≤ j ≤ k . Use triples to generate the stream of all
;; Pythagorean triples of positive integers, i.e., the triples (i, j, k)
;; such that i ≤ j and i^2 + j^2 = k^2.

(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-map (lambda (x) (cons (stream-car s) x))
                (pairs (stream-cdr t) (stream-cdr u)))
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))
