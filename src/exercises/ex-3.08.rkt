#lang racket

;; exercise 3.8 -- define a procedure where result depends on evaluation order
(define (make-f)
  (let ((state '()))
    (define (g a)
      (if (null? state)
          (begin
            (set! state a)
            a)
          (begin
            (set! state (xor state a))
            (if state 1 0))))
    g))


;; tests
(define fa (make-f))
(define fb (make-f))
(+ (fa 0) (fa 1))                       ; "left to right"
(+ (fb 1) (fb 0))                       ; "right to left"
