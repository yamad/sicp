#lang racket

;; exercise 3.5 -- Monte Carlo integration
;; estimate pi by measuring area of unit circle

(define (estimate-pi trials)
  (/ (estimate-integral in-circle? 1 2 1 2 trials)
     (expt 0.5 2)))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (monte-carlo trials (lambda () (P x1 x2 y1 y2))))

(define (in-circle? x1 x2 y1 y2)
  (let* ((cx (midpoint x1 x2))
         (cy (midpoint y1 y2))
         (x-rand (random-in-range x1 x2))
         (y-rand (random-in-range y1 y2))
         (r (- cx (min x1 x2))))
    (<= (+ (expt (- x-rand cx) 2)
           (expt (- y-rand cy) 2))
        (expt r 2))))

(define (midpoint a b)
  (/ (+ a b) 2))
;; helper procedure from book (p. 229)
(define (random-in-range low high)
  (let ((range (exact->inexact (- high low))))
    (+ low (* range (random)))))

;; monte-carlo procedure from book (p. 227)
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))


;; test
(define (avg-experiments nexps)
  (define (iter exps-rem acc)
    (if (= exps-rem 0) acc
        (iter (- exps-rem 1)
              (+ acc (estimate-pi 10000)))))
  (/ (iter nexps 0) nexps))
(display "trials  estimate of pi") (newline)
(display "   10:  ") (estimate-pi 10)    (newline)
(display "  100:  ") (estimate-pi 100)   (newline)
(display " 1000:  ") (estimate-pi 1000)  (newline)
(display "10000:  ") (estimate-pi 10000) (newline)
(display "10000:  ") (estimate-pi 10000) (newline)

(display "avg (100 expts, 10000 sims): ")(newline)
(avg-experiments 100)
