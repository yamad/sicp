#lang racket

;; continued fraction calculator - iterative version
(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (zero? i)
        result
        (iter (- i 1)
              (/ (n i)
                 (+ (d i) result)))))
  (iter k 0))
(define (square x) (* x x))

;; exercise 1.39
(define (tan-cf x k)
  (define (n i)
    (if (= i 1)
        x
        (- (square x))))
  (define (d i)
    (- (* 2 i) 1))
  (cont-frac-iter n d k))

(tan (/ pi 6))
(tan-cf (/ pi 6) 10)

