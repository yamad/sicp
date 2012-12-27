#lang racket

(define (last-pair l)
  (if (or (null? l)
          (null? (cdr l)))
      l
      (last-pair (cdr l))))

(define (last-pair-safe l)
  (define (last-pair-iter items result)
    (if (null? items)
        result
        (last-pair-iter (cdr items) items)))
  (last-pair-iter l l))

(last-pair (list 1 2 3 4)) "should be" (list 4)
(last-pair '()) "should be" '()

(last-pair-safe (list 1 2 3 4)) "should be" (list 4)
(last-pair-safe '()) "should be" '()
