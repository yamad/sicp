#lang racket

;; exercise 2.29 -- return left-to-right leaves of tree
(define (fringe tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (list tree))
        (else
         (append (fringe (car tree))
                 (fringe (cdr tree))))))

(define (fringe-iter tree)
  (define (iter root res)
    (cond ((null? root) res)
          ((not (pair? root)) (cons root res))
          (else
           (iter (car root) (iter (cdr root) res)))))
  (iter tree '()))

(define x (list (list 1 2) (list 3 4)))

(fringe x)
(fringe (list x x))

(fringe-iter x)
(fringe-iter (list x x))
