#lang racket

(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (accumulate op initial (cdr seq)))))

(define (map-jyh p sequence)
  (accumulate
   (lambda (x y)
     (cons (p x) y)) null sequence))

(define (append-jyh seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length-jyh sequence)
  (accumulate (lambda (x y)
                (+ 1 y))
              0 sequence))

;; test map
(define (inc x) (+ 1 x))
(map inc '(1 2 3))
(map-jyh inc '(1 2 3))

;; test append
(append '(1 2 3) '(4 5 6))
(append-jyh '(1 2 3) '(4 5 6))

;; test length
(length '(1 2 3))
(length-jyh '())
(length-jyh '(1))
(length-jyh '(1 2))
(length-jyh '(1 2 3))
