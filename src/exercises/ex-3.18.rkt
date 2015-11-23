#lang r5rs
;; exercise 3.18 -- detect cycles in lists

;; detect a cycle in list. uses depth-first search
(define (cycle? l)
  (define (dfs rest marked)
    (cond ((null? rest) #f)               ; reached the end with no cycles
          ((member (car rest) marked) #t) ; found cycle
          ((not (pair? (car rest)))
           (dfs (cdr rest) (cons (car rest) marked)))
          (else
           (or (dfs (car rest) (cons (car rest) marked))
               (dfs (cdr rest) (cons (car rest) marked))))))
  (dfs l '()))

;; test
(define a
  (let* ((z (list 'c))
         (y (cons 'b z))
         (x (cons 'a y)))
    x))

(define b
  (let* ((z (list 'c))
         (y (cons z z))
         (x (cons 'a y)))
    x))

(define c
  (let* ((z (list 'c))
         (y (cons z z))
         (x (cons y y)))
    x))

(define d
  (let* ((z (list 'c))
         (y (cons z z))
         (x (cons y y)))
    (set-cdr! z x)
    x))

(define e
  (let* ((d (list 'd))
         (c (cons d d))
         (b (cons d c))
         (a (cons b b)))
    (set-car! b (cons 'x a))
    a))

(define (println a)
  (display a) (newline))

(println (cycle? a))
(println (cycle? b))
(println (cycle? c))
(println (cycle? d))
(println (cycle? e))
