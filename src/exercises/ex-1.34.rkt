#lang racket

;; exercise 1.34

;; given the procedure f
(define (f g)
  (g 2))

;; what happens if f is an argument to itself?
(f f)

;; an error occurs because 2 is applied as a procedure to 2
;; on the first call, g is f. Using substition,
(f f)
(f 2)

;; in (f 2), g is 2, so using substitution again,
(2 2)

;; this file will result in error