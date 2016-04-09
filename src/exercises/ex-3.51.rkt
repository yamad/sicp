#lang racket
(require "../examples/streams.rkt")
;; Exercise 3.51: In order to take a closer look at delayed evaluation,
;; we will use the following procedure, which simply returns its argument
;; after printing it:

(define (show x)
    (displayln x)
    x)

;; What does the interpreter print in response to evaluating each
;; expression in the following sequence?

(define x
    (stream-map show
        (stream-enumerate-interval 0 10)))
;; 0

(stream-ref x 5)
;; 1
;; 2
;; 3
;; 4
;; 5
;; returns: 5

(stream-ref x 7)
;; 6
;; 7
;; returns: 7

;; (note that this uses the code directly from the book, modified as
;; little as possible to work within Racket. Racket streams seem to
;; work differently, and to evaluate arguments at different times)

;; The key insight is that `stream-map` evaluates its procedure on the
;; head of the stream immediately but defers all other
;; evaluations. Thus, when `x` is constructed, `show` is called on the
;; head of `stream-enumerate-interval`, which in turn displays the
;; head just before cons'ing with the rest of the delayed stream.

;; trace:
;; (stream-map show (stream-enumerate-interval 0 10)))
;; (stream-map show (cons-stream 0 (stream-enumerate-interval 1 10)))
;; (cons-stream (show 0) (stream-map show (stream-enumerate-interval 1 10)))
;; [output: 0]
;; (cons-stream 0 (stream-map show (stream-enumerate-interval 1 10)))
