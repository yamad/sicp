#lang racket
(require "../examples/streams.rkt")
(require "ex-3.50.rkt")

;; Exercise 3.79: Generalize the `solve-2nd' procedure of Exercise
;; 3-78 so that it can be used to solve general second-order
;; differential equations d^2 y/dt^2 = f(dy/dt, y).

(define (solve-2nd f dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map-multiple f dy y))
  y)
