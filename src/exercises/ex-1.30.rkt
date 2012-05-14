#lang racket

;; exercise 1.30
;; implement the sum procedure as an iterative process
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))



;; test on simpson-integral
(define (cube x) (* x x x))
(define (ensure-even x)
  (if (even? x) x
      (+ x 1)))
(define (simpson-integral-iter f a b n)
  (define fixed-n (ensure-even n))
  (define h (/ (- b a) fixed-n))
  (define (yk k) (f (+ a (* k h))))
  (define (inc x) (+ x 1))
  (define (simpson-term k)
    (* (yk k)
       (cond ((or (= k 0) (= k fixed-n)) 1.0)
             ((even? k) 2.0)
             (else 4.0))))
  (* (/ h 3.0)
     (sum-iter simpson-term 0 inc fixed-n)))

(simpson-integral-iter cube 0 1 100)
(simpson-integral-iter cube 0 1 1000)


