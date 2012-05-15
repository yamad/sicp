#lang racket

;; exercise 1.36

;; fixed-point approximation with printing
(define (print-line x)
  (display x)
  (newline))
(define tolerance 0.00001)
(define (fixed-point-with-print f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (print-line guess)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;; fixed point of x^x = 1000
;; solve by x --> log(1000) / log(x)
(define (x-to-the-x y)
  (fixed-point-with-print (lambda (x) (/ (log y) (log x))) 2.0))

;; with average damping
(define (average x y) (/ (+ x y) 2.0))
(define (x-to-the-x-avg y)
  (fixed-point-with-print (lambda (x) (average x (/ (log y) (log x)))) 2.0))

(x-to-the-x 1000)
(newline)
(print-line "with average damping")
(print-line "====================")
(x-to-the-x-avg 1000)