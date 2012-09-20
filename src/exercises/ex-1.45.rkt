#lang racket

;; ex 1.45

;; find the n-th root of x
(define (root-n x n)
  (fixed-point ((repeated average-damp (floor (log2 n)))
                (lambda (y) (/ x (expt y (- n 1)))))
               1.0))

(define (log2 x)
  (/ (log x) (log 2)))

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

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? a b)
    (< (abs (- a b)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? next guess)
          next
          (try next))))
  (try first-guess))

(define (average a b)
  (/ (+ a b) 2))
(define (average-damp f)
  (lambda (x) (average x (f x))))

(root-n 2 4)
(root-n 2 7)
