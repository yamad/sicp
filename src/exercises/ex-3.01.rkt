#lang racket
;; exercise 3.1 -- make an accumulator generator

(define (make-accumulator sum)
  (lambda (addend)
    (begin (set! sum (+ sum addend))
           sum)))


;; tests (from book)
(define A (make-accumulator 5))
(A 10)                                  ; 15
(A 10)                                  ; 25
