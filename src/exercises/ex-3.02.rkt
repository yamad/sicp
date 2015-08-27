#lang racket

;; exercise 3.2 -- instrument functions with counter
(define (make-monitored f)
  (let ((ncalls 0))
    (lambda (m)
      (cond ((eq? m 'how-many-calls?) ncalls)
            ((eq? m 'reset-count) (set! ncalls 0))
            (else
             (set! ncalls (+ ncalls 1))
             (f m))))))


;; test
(define s (make-monitored sqrt))
(s 'how-many-calls?)                    ; 0
(s 100)                                 ; 100
(s 'how-many-calls?)                    ; 1
