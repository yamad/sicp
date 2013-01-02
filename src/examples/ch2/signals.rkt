#lang racket

(define (product-of-squares-of-odd-elements seq)
  (accumulate *
              1
              (map square
                   (filter odd? seq))))

(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (accumulate op initial (cdr seq)))))

(define (square x) (* x x))

(product-of-squares-of-odd-elements '(1 2 3 4 5))
