#lang racket

(define (for-each proc items)
  (cond ((null? items) #t)
        (else
         (proc (car items))
         (for-each proc (cdr items)))))

(for-each (lambda (x) (newline) (display x))
          (list 47 321 88))
