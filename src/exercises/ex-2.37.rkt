#lang racket

;; exercise 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (a)
         (dot-product a v))
       m))

(define (sum-seq seq)
  (accumulate + 0 seq))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (a)
           (matrix-*-vector cols a)) m)))

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init
                        (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define m '((1 2 3 4)
            (4 5 6 6)
            (6 7 8 9)))

(define n '((1 2 3)
            (4 5 6)
            (7 8 9)))

m
(dot-product (car m) (cadr m))
(sum-seq '(1 2 3))
(sum-seq (map (lambda (x) (* 2 x)) '(1 2 3)))
(matrix-*-vector n '(1 2 3))
(transpose m)
(matrix-*-matrix n (transpose n))
