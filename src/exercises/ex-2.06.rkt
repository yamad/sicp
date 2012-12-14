#lang racket

;; ex 2.06 -- Church numerals
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;; one and two defined "directly"
;; (repeated application of f to x)
(define one
  (lambda (f) (lambda (x) (f x))))
(define two
  (lambda (f) (lambda (x) (f (f x)))))

;; add two Church numerals
(define (add a b)
  (lambda(f) (lambda (x) ((a f) ((b f) x)))))



;; increment test function
(define (inc a)
  (+ a 1))

((zero inc) 1)
((one inc) 1)
((two inc) 1)
(((add one two) inc) 1)
