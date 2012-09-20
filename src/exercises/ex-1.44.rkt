#lang racket

(define dx 0.0001)

;; ex 1.44
(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx))
                    (f x)
                    (f (+ x dx)))
                 3)))

(define (smooth-n f n)
  ((repeated smooth n) f))


(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (rep-iter f g n)
    (if (= n 1)
        g
        (rep-iter
         f
         (compose f g)
         (- n 1))))
  (rep-iter f f n))


(define (g-test x)
  (+ (* x 2) 1))

(g-test 2)
((smooth g-test) 2)
((smooth-n g-test 2) 2)
