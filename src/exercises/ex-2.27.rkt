#lang racket

;; exercise 2.27 -- recursive reverse
(define (deep-reverse l)
  (define (iter items result)
    (cond ((null? items) result)
          ((not (pair? items)) items)
          (else
           (iter (cdr items)
                 (cons (deep-reverse (car items))
                       result)))))
  (iter l '()))

(deep-reverse (list (list 1 2) (list 3 4) 5 6))
