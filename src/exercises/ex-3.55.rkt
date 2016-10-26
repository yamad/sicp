#lang racket
(require "../examples/streams.rkt")
;; Exercise 3.55: Define a procedure partial-sums that takes as argument
;; a stream S and returns the stream whose elements are S0,
;; S0 + S1, S0 + S1 + S2, . . .. For example, (partial-sums integers)
;; should be the stream 1, 3, 6, 10, 15, . . . .

(define (partial-sums stream)
  (if (stream-null? stream)
      the-empty-stream
      (let ((head (stream-car stream))
            (rest (stream-cdr stream)))
        (cons-stream head
                     (add-streams (repeat head) (partial-sums rest))))))

(provide partial-sums)
