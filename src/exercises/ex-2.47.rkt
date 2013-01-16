#lang racket

(define (make-frame-l origin edge1 edge2)
  (list origin edge1 edge2))

(define (make-frame-c origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame-l f)
  (car f))
(define (edge1-frame-l f)
  (car (cdr f)))
(define (edge2-frame-l f)
  (car (cdr (cdr f))))

(define (origin-frame-c f)
  (car f))

(define (edge1-frame-c f)
  (car (cdr f)))

(define (edge2-frame-c f)
  (cdr (cdr f)))

(define fl (make-frame-l 0 0.5 0.7))
(define fc (make-frame-c 0.1 0.2 0.3))

(origin-frame-l fl)
(edge1-frame-l fl)
(edge2-frame-l fl)

(origin-frame-c fc)
(edge1-frame-c fc)
(edge2-frame-c fc)
