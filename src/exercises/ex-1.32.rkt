#lang racket

;; exercise 1.32
;; iterative accumulator
(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

;; recursive accumulator
(define (accumulate-rec combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
          (accumulate-rec combiner null-value term (next a) next b))))

(define (sum-rec term a next b)
  (accumulate-rec + 0 term a next b))
(define (sum-iter term a next b)
  (accumulate-iter + 0 term a next b))

(define (product-rec term a next b)
  (accumulate-rec * 1 term a next b))
(define (product-iter term a next b)
  (accumulate-iter * 1 term a next b))

(define (identity x) x)
(define (inc x) (+ x 1))
(sum-rec identity 1 inc 4)
(sum-iter identity 1 inc 4)
(product-rec identity 1 inc 4)
(product-iter identity 1 inc 4)