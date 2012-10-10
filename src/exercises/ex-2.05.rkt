#lang racket

;; ex 2.05 -- represent pair (a b) as 2^a * 3^b
(define (div-num val div)
  (define (iter n c)
    (if (= (remainder n div) 0)
        (iter (/ n div)
              (+ c 1))
        c))
  (iter val 0))

(define (cons-int a b)
  (* (expt 2 a) (expt 3 b)))
(define (car-int z) (div-num z 2))
(define (cdr-int z) (div-num z 3))

(car-int (cons-int 9 4))
(cdr-int (cons-int 9 4))
