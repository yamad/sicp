#lang racket

;; ex 2.02

(define (midpoint-segment s)
  (make-point
   (/ (+ (x-point (start-segment s))
         (x-point (end-segment s)))
      2)
   (/ (+ (y-point (start-segment s))
         (y-point (end-segment s)))
      2)))

;; segments
(define (make-segment pa pb)
  (cons pa pb))
(define (start-segment s)
  (car s))
(define (end-segment s)
  (cdr s))

;; points
(define (make-point a b)
  (cons a b))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))


(define segment-a
  (make-segment
   (make-point 2 0)
   (make-point 2 5)))

(print-point (midpoint-segment segment-a))
