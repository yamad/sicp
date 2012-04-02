#lang racket/base

;; Returns the value of the nth fibonacci number
;; iterative algorithm
(define (fib-iter n)
  (define (iter a b n)
    (if (= n 0) b
        (iter b (+ a b) (- n 1))))

  ; provide F0 = 0, and F1 = 1
  (cond ((<= n 0) 0)
        ((= n 1) 1)
        (else (iter 0 1 (- n 1)))))


;; tests

; print ascending fibonacci sequence
(define (run-fib n)
  (define (iter n i)
    (if (> i n) (void)
        (begin
          (display (fib-iter i)) (display "\n")
          (iter n (+ i 1)))))
  (iter n 0))

(require rackunit)
(check-eq? (fib-iter -1) 0)
(check-eq? (fib-iter 0) 0)
(check-eq? (fib-iter 1) 1)
(run-fib 10)