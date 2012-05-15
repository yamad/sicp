#lang racket

;; exercise 1.31
;; calculate product over range a,b - recursive
(define (product-rec term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-rec term (next a) next b))))

;; calculate product over range a,b - iterative
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))


;; support functions
(define (identity x) x)
(define (inc x) (+ x 1))
(define (square x) (* x x))

;; factorial in terms of product
(define (factorial n)
  (product-iter identity 1 inc n))
(define (factorial-rec n)
  (product-rec identity 1 inc n))

(factorial 1)                           ; --> 1
(factorial 2)                           ; --> 2
(factorial 3)                           ; --> 6
(factorial 4)                           ; --> 24
(factorial-rec 4)                           ; --> 24

;; Wallis approximation to pi
;; latex: \prod_{i=1}^{n} \frac{2k(2(k+1))}{ (2k+1)^2 }
(define (pi-approx n)
  (define (numer k) (* (* 2.0 k) (* 2.0 (+ k 1))))
  (define (denom k) (square (+ (* 2.0 k) 1)))
  (define (yk k) (/ (numer k) (denom k)))
  (* 4 (product-iter yk 1.0 inc n)))

(pi-approx 100)                         ; accurate to 3 digits
(pi-approx 10000)                       ; accurate to 4 digits
(pi-approx 1000000)                     ; accurate to 6 digits
