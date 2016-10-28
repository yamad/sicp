#lang racket
(require "../examples/streams.rkt")

;; Exercise 3.67: Modify the pairs procedure so that (pairs integers
;; integers) will produce the stream of all pairs of integers (i, j)
;; (without the condition i ≤ j ). Hint: You will need to mix in an
;; additional stream.

(define (all-pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))        ; (s0, t0)
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))                ; (s0, t1), (s0, t2), ...
    (all-pairs (stream-cdr s) t))))            ; (s1, t0), (s1, t1), ...



;; alternate elements from two streams
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

;; for testing
(define (cmp-list a b)
  (cond ((null? a) #t)
        ((null? b) #f)
        ((< (car a) (car b)) #t)
        ((> (car a) (car b)) #f)
        (else
         (cmp-list (cdr a) (cdr b)))))
