#lang racket

;; exercise 3.6 -- design resettable rand procedure
(define (rand m)
  (cond ((eq? m 'generate) (random))
        ((eq? m 'reset) (lambda (x)
                          (random-seed x)))
          (else
           (error "Unknown request -- RAND:" m))))

;; test
((rand 'reset) 123)
(rand 'generate)
(rand 'generate)
((rand 'reset) 123)
(rand 'generate)
(rand 'generate)
