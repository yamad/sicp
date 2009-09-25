#!/usr/bin/env mzscheme
#lang scheme

;; ex 1.8
;; ------

;; An implementation of Newton's method of approximation for cube roots
(define (square x) (* x x))

(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (good-enough? guess last-guess)
  (< (abs (- guess last-guess)) (* guess 0.001)))

(define (cuberoot-iter guess last-guess x)
  (if (good-enough? guess last-guess)
      guess
      (cuberoot-iter (improve guess x)
                     guess
                     x)))

(define (cuberoot x)
  (cuberoot-iter 5.0 0.0 x))

;; test
(cuberoot 2)
;; expected: 1.259