#lang racket
(require "../examples/streams.rkt")
(require "ex-3.70.rkt")

;; Exercise 3.71: Numbers that can be expressed as the sum of two
;; cubes in more than one way are sometimes called "Ramanujan
;; numbers", in honor of the mathematician Srinivasa Ramanujan.(6)
;; Ordered streams of pairs provide an elegant solution to the problem
;; of computing these numbers.  To find a number that can be written
;; as the sum of two cubes in two different ways, we need only
;; generate the stream of pairs of integers (i,j) weighted according
;; to the sum i^3 + j^3 (Exercise 3-70), then search the stream for
;; two consecutive pairs with the same weight.  Write a procedure to
;; generate the Ramanujan numbers.  The first such number is 1,729.
;; What are the next five?

(define (sum-of-cubes x y)
  (+ (expt x 3)
     (expt y 3)))

(define candidate-pairs
  (weighted-pairs integers integers
                  (λ (x) (apply sum-of-cubes x))))

(define (find-ramanujan-number s)
    (let* ((s0 (stream-ref s 0))
           (s1 (stream-ref s 1))
           (c0 (apply sum-of-cubes s0))
           (c1 (apply sum-of-cubes s1)))
      (if (= c0 c1)
          (cons-stream (list c0 s0 s1)
                       (find-ramanujan-number (stream-cdr s)))
          (find-ramanujan-number (stream-cdr s)))))

(define ramanujan-numbers
  (find-ramanujan-number candidate-pairs))
