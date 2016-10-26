#lang racket

(require "../examples/streams.rkt")
;; stream that emits powers of two
(define s (cons-stream 1 (add-streams s s)))
