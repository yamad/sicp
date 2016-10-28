#lang racket
(require "../examples/streams.rkt")
(require "ex-3.70.rkt")

;; Exercise 3.72: In a similar way to Exercise 3-71 generate a stream
;; of all numbers that can be written as the sum of two squares in
;; three different ways (showing how they can be so written).

(define (sum-of-squares x y)
  (+ (expt x 2)
     (expt y 2)))


(define (find-ss-triples s)
    (let* ((s0 (stream-ref s 0))
           (s1 (stream-ref s 1))
           (s2 (stream-ref s 2))
           (c0 (apply sum-of-squares s0))
           (c1 (apply sum-of-squares s1))
           (c2 (apply sum-of-squares s2)))
      (if (= c0 c1 c2)
          (cons-stream (list c0 s0 s1 s2)
                       (find-ss-triples (stream-cdr s)))
          (find-ss-triples (stream-cdr s)))))

(define ordered-candidate-pairs
  (weighted-pairs integers integers
                  (λ (x) (apply sum-of-squares x))))
(define sum-of-squares-triples
  (find-ss-triples ordered-candidate-pairs))
