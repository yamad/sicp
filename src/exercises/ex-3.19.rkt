#lang r5rs
;; exercise 3.19 -- detect cycles in lists in constant space

;; use Floyd's tortoise and hare algorithm
;;
;; - tortoise steps through elements one-by-one
;; - hare steps through elements two-by-two
;; - if the tortoise and hare end up in the same place,
;;     then there is a cycle
(define (cycle? l)
  (define (safe-cdr lst)
    (if (pair? lst)
        (cdr lst)
        '()))
  (define (floyd tort hare)
    (cond ((null? tort) #f)
          ((null? hare) #f)
          ((eq? (car tort) (car hare)) #t)
          (else
           (floyd (safe-cdr tort)
                  (safe-cdr (safe-cdr hare))))))
    (floyd (safe-cdr l) (safe-cdr (safe-cdr l))))

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
    (set-cdr! c (cons 'x a))
    a))

(define (println a)
  (display a) (newline))

(println (cycle? a))
(println (cycle? b))
(println (cycle? c))
(println (cycle? d))
(println (cycle? e))
