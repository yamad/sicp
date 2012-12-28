#lang racket

;; exercise 2.22
(define (square x) (* x x))

(define (square-list l)
  (define (iter items result)
    (if (null? items)
        result
        (iter (cdr items)
              (append result
                      (list (square (car items)))))))
  (iter l '()))

(square-list (list 1 2 3 4))
