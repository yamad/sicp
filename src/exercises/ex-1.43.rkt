#lang racket

;; ex 1.43
(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (rep-iter f g n)
    (if (= n 1)
        g
        (rep-iter
         f
         (compose f g)
         (- n 1))))
  (rep-iter f f n))

(define (square x) (* x x))
(define (inc x) (+ x 1))

((repeated square 2) 5)
((repeated inc 3) 0)
