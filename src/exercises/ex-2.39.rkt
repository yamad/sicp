#lang racket

;; exercise 2.39

(define (reverse-r seq)
  (fold-right (lambda (x y)
                (append y (list x)))
              null seq))

(define (reverse-l seq)
  (fold-left (lambda (x y)
               (cons y x))
             null seq))

(define (fold-right op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (fold-right op init (cdr seq)))))

(define (fold-left op init seq)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter init seq))

(reverse-r '())
(reverse-r '(1 2 3))
(reverse-l '(1 2 3))
