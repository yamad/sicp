#lang racket

;; exercise 2.18 -- reverse a list
(define (reverse-jyh l)
  (define (iter items result)
    (if (null? items)
        result
        (iter (cdr items)
              (cons (car items) result))))
  (iter l '()))

(reverse-jyh '())
(reverse-jyh '(1))
(reverse-jyh '(1 2))
(reverse-jyh (list 1 4 9 16 25))
