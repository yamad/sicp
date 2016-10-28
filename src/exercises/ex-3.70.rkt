#lang racket
(require "../examples/streams.rkt")
(require "ex-3.66.rkt")

;; Exercise 3.70: It would be nice to be able to generate streams in
;; which the pairs appear in some useful order, rather than in the
;; order that results from an _ad hoc_ interleaving process.  We can
;; use a technique similar to the `merge' procedure of Exercise 3-56,
;; if we define a way to say that one pair of integers is "less than"
;; another.  One way to do this is to define a "weighting function"
;; W(i,j) and stipulate that (i_1, j_1) is less than (i_2, j_2) if
;; W(i_1, j_1) < W(i_2, j_2).
;;
;; Write a procedure `merge-weighted' that is like `merge', except
;; that `merge-weighted' takes an additional argument `weight', which
;; is a procedure that computes the weight of a pair, and is used to
;; determine the order in which elements should appear in the
;; resulting merged stream.

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let* ((s1car (stream-car s1))
                (s2car (stream-car s2))
                (w1 (weight s1car))
                (w2 (weight s2car)))
           (cond ((< w1 w2)
                  (cons-stream s1car
                               (merge-weighted (stream-cdr s1) s2 weight)))
                 ((> w1 w2)
                  (cons-stream s2car
                               (merge-weighted s1 (stream-cdr s2) weight)))
                 (else
                  (cons-stream s1car
                               (merge-weighted
                                (stream-cdr s1) s2
                                weight))))))))

;; Using this, generalize `pairs' to a procedure `weighted-pairs' that
;; takes two streams, together with a procedure that computes a
;; weighting function, and generates the stream of pairs, ordered
;; according to weight.

(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
    weight)))

;; Use your procedure to generate
;;
;; a. the stream of all pairs of positive integers (i,j) with i <= j
;; ordered according to the sum i + j

(define pairs-by-sum
  (weighted-pairs integers integers
                  (lambda (x) (apply + x))))

;; b. the stream of all pairs of positive integers (i,j) with i <=
;; j, where neither i nor j is divisible by 2, 3, or 5, and the
;; pairs are ordered according to the sum 2 i + 3 j + 5 i j.

(define (divisible? x y) (= (remainder x y) 0))
(define (div-test x)
  (not (or (divisible? x 2)
           (divisible? x 3)
           (divisible? x 5))))
(define integers-not235
  (stream-filter div-test integers))
(define (poly-sum i j)
  (+ (* 2 i) (* 3 j) (* 5 i j)))

(define pairs2
  (weighted-pairs integers-not235 integers-not235
                  (lambda (x) (apply poly-sum x))))


(provide weighted-pairs)
