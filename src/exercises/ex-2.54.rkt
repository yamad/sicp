#lang racket

(define (equal-jyh? a b)
  (cond ((and (null? a) (null? b)) #t)
        ((or (null? a) (null? b)) #f)
        ((and (symbol? a)
              (symbol? b))
         (eq? a b))
        ((and (list? a)
              (list? b))
         (and (equal? (car a) (car b))
              (equal? (cdr a) (cdr b))))))

;; should be true
(equal-jyh? '() '())
(equal-jyh? '(a b c) '(a b c))
(equal-jyh? '(this is a list) '(this is a list))

;; should be false
(equal-jyh? '(1 2 3) '(1 a b))
(equal-jyh? '() '(a))
(equal-jyh? '(a b) '())
(equal-jyh? '(this is a list) '(this (is a) list))
