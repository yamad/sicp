#lang racket
(require (planet soegaard/sicp:2:1/sicp))

(define bl (make-vect 0 0))
(define tl (make-vect 0 0.99))
(define tr (make-vect 0.99 0.99))
(define br (make-vect 0.99 0))

(define frame-outline
  (segments->painter
   (list (make-segment bl tl)
         (make-segment tl tr)
         (make-segment tr br)
         (make-segment br bl))))

(define paint-x
  (segments->painter
   (list (make-segment bl tr)
         (make-segment br tl))))

(define diamond
  (let ((left (make-vect 0 0.5))
        (right (make-vect 0.999 0.5))
        (top (make-vect 0.5 0.999))
        (bottom (make-vect 0.5 0)))
    (segments->painter
     (list (make-segment left top)
           (make-segment top right)
           (make-segment right bottom)
           (make-segment bottom left)))))

(paint frame-outline)
(paint paint-x)
(paint diamond)
