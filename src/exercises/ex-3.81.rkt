#lang racket
(require "../examples/streams.rkt")

;; Exercise 3.81: Exercise 3-6 discussed generalizing the
;; random-number generator to allow one to reset the random-number
;; sequence so as to produce repeatable sequences of "random" numbers.
;; Produce a stream formulation of this same generator that operates
;; on an input stream of requests to `generate' a new random number or
;; to `reset' the sequence to a specified value and that produces the
;; desired stream of random numbers.  Don't use assignment in your
;; solution.

(define (random-stream request-stream)
  (if (stream-null? request-stream)
      the-empty-stream
      (let ((m (stream-car request-stream))
            (rest (stream-cdr request-stream)))
        (cond ((eq? m 'generate)
               (cons-stream (random) (random-stream rest)))
              ((and (eq? m 'reset)
                    (not (stream-null? rest)))
               (cons-stream (random-seed (stream-car rest))
                            (random-stream (stream-cdr rest))))
              (else (error "Invalid request stream -- RANDOM-STREAM"))))))


;; tests
(define requests
  (list-to-stream '(reset 123 generate generate reset 123 generate generate)))
(display-stream (random-stream requests))


(provide random-stream)
