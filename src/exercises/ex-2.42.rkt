#lang racket

;; exercise 2.42 - eight queens
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board '())

(define (safe? k pos)
  (define (safe-iter new n pos)
    (cond ((null? pos) #t)
          ((or (= (car pos) new)
               (and (= k n)             ; check diagonals on first iter
                    (or (= (car pos) (+ new 1))
                        (= (car pos) (- new 1)))))
           #f)
          (else
           (safe-iter new (- n 1) (cdr pos)))))
  (safe-iter (car pos) k (cdr pos)))

(define (adjoin-position new k rest)
  (cons new rest))

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


(queens 4)
(queens 5)
(queens 6)
