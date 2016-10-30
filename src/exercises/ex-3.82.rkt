#lang racket
(require "../examples/streams.rkt")
(require "ex-3.81.rkt")

;; Exercise 3.82: Redo Exercise 3-5 on Monte Carlo integration in
;; terms of streams.  The stream version of `estimate-integral' will
;; not have an argument telling how many trials to perform.  Instead,
;; it will produce a stream of estimates based on successively more
;; trials.

(define random-numbers (random-stream (repeat 'generate)))

(define (make-in-circle? x1 x2 y1 y2)
  (define (midpoint a b)
    (/ (+ a b) 2))
  (define (in-circ? r1 r2)
    (let* ((cx (midpoint x1 x2))
           (cy (midpoint y1 y2))
           (x-rand (to-range x1 x2 r1))
           (y-rand (to-range y1 y2 r2))
           (r (- cx (min x1 x2))))
      (<= (+ (expt (- x-rand cx) 2)
             (expt (- y-rand cy) 2))
          (expt r 2))))
  in-circ?)

(define (estimate-pi trials)
  (/ (stream-ref (estimate-integral make-in-circle? 1 2 1 2) trials)
     (expt 0.5 2)))

(define (estimate-integral P x1 x2 y1 y2)
  (let ((p? (P x1 x2 y1 y2)))
    (monte-carlo
     (map-successive-pairs p? random-numbers)
     0 0)))

;; adapted from random-in-range
(define (to-range low high val)
  (let ((range (exact->inexact (- high low))))
    (+ low (* range val))))

;; monte-carlo procedure from book (p. 353)
(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

;; from p. 353
(define (map-successive-pairs f s)
  (cons-stream
   (f (stream-car s) (stream-car (stream-cdr s)))
   (map-successive-pairs f (stream-cdr (stream-cdr s)))))


(display "trials  estimate of pi") (newline)
(display "    10: ") (estimate-pi 10)     (newline)
(display "   100: ") (estimate-pi 100)    (newline)
(display "  1000: ") (estimate-pi 1000)   (newline)
(display " 10000: ") (estimate-pi 10000)  (newline)
(display "100000: ") (estimate-pi 100000) (newline)
