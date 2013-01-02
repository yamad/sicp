#lang racket

;; exercise 2.35
(define (count-leaves t)
  (accumulate (lambda (x y) (+ 1 y))
              0
              (enumerate-tree t)))

(define (count-leaves-alt t)
  (accumulate + 0
              (map (lambda (x) 1)
                   (enumerate-tree t))))

(define (enumerate-tree tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (accumulate op initial (cdr seq)))))

(count-leaves (list (list 1 (list 2 3) 4 5) 6))
(count-leaves-alt (list (list 1 (list 2 3) 4 5) 6))
