#lang racket
(require "../examples/streams.rkt")

;; Exercise 3.64: Write a procedure stream-limit that takes as argu-
;; ments a stream and a number (the tolerance). It should examine the
;; stream until it ﬁnds two successive elements that differ in
;; absolute value by less than the tolerance, and return the second of
;; the two elements. Using this, we could compute square roots up to a
;; given tolerance by
;;
;; (define (sqrt x tolerance)
;;   (stream-limit (sqrt-stream x) tolerance))

(define (stream-limit stream tolerance)
  (let ((s0 (stream-ref stream 0))
        (s1 (stream-ref stream 1)))
    (if (< (abs (- s1 s0)) tolerance)
        s1
        (stream-limit (stream-cdr stream) tolerance))))

(provide stream-limit)
