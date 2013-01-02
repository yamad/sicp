#lang racket

;; exercise 2.41
(define (triples-of-n-add-to-s n s)
  (filter (lambda (triple) (sum-to? s triple))
          (unique-triples n)))

(define (sum-to? s seq)
  (= s (fold-right + 0 seq)))

(define (unique-triples n)
  (flatmap (lambda (i)
             (map (lambda (j)
                    (cons i j))
                  (unique-pairs (- i 1))))
           (enumerate-interval 1 n)))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (flatmap proc seq)
  (fold-right append null (map proc seq)))

(define (fold-right op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (fold-right op init (cdr seq)))))

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))

(unique-triples 4)
(sum-to? 4 '(1 1 2))
(sum-to? 4 '(1 2 3))
(triples-of-n-add-to-s 4 8)
