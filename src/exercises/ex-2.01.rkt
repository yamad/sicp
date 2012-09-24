#lang racket

;; ex 2.1
;; enhance make-rat to normalize negative rational numbers

(define (make-rat n d)
  (let ((g (gcd (abs n) (abs d))))
    (cons
     ((if (same-sign? n d) + -)
      (/ (abs n) g))
     (/ (abs d) g))))

(define (same-sign? a b)
  (> (* a b) 0))

(define (gcd a b)
  (let ((a (abs a))
        (b (abs b)))
    (if (= b 0)
        a
        (gcd b (remainder a b)))))

(define (numer x) (car x))
(define (denom x) (cdr x))
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(print-rat (make-rat 1 2))
(print-rat (make-rat -1 -2))
(print-rat (make-rat -1 2))
(print-rat (make-rat 1 -2))
