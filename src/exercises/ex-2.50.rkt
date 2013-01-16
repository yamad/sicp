#lang racket
(require (planet soegaard/sicp:2:1/sicp))

(define (flip-horiz-jyh painter)
  ((transform-painter
    (make-vect 1.0 0)
    (make-vect 0 0)
    (make-vect 1.0 1.0))
   painter))

(define (rotate180-jyh painter)
  ((transform-painter
    (make-vect 1.0 1.0)
    (make-vect 0 1.0)
    (make-vect 1.0 0))
   painter))

(define (rotate270-jyh painter)
  ((transform-painter
    (make-vect 0 1.0)
    (make-vect 0 0)
    (make-vect 1.0 1.0))
   painter))

(paint einstein)
(paint (flip-horiz-jyh einstein))
(paint (rotate180-jyh einstein))
(paint (rotate270-jyh einstein))
