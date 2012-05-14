#lang racket

;; summation as a higer-order procedure
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

;; sum of cubes
(define (cube x) (* x x x))
(define (inc x) (+ x 1))
(define (sum-cubes a b)
  (sum cube a inc b))

(sum-cubes 1 3)                         ; --> 1 + 8 + 27 = 36


;; sum of integers
(define (identity x) x)
(define (sum-integers a b)
  (sum identity a inc b))

(sum-integers 1 3)                      ; --> 1 + 2 + 3 = 6


;; Leibniz pi/8 series
(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(* 8 (pi-sum 1 1000))                   ; approx to pi


;; definite integral approximation
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(integral cube 0 1 0.01)                ; ~ .2499875
(integral cube 0 1 0.001)               ; ~ .249999875