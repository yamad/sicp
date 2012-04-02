#lang racket/base

;; ex 1.19
;; with a lot of help from the internet

;; A fibonacci generator based on successive "squaring" fibonacci transformation
;; p' = p^2 + q^2
;; q' = q^2 + 2pq
;; see notes for details
(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q)) ; compute p'
                   (+ (square q) (* 2 p q))  ; compute q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

(define (square x) (* x x))

;; tests
(require rackunit)
(check-eq? (fib 0) 0)
(check-eq? (fib 1) 1)
(check-eq? (fib 2) 1)
(check-eq? (fib 3) 2)
(check-eq? (fib 4) 3)
(check-eq? (fib 5) 5)
(check-eq? (fib 6) 8)
(check-eq? (fib 7) 13)
(check-eq? (fib 8) 21)
(check-eq? (fib 9) 34)
(check-eq? (fib 10) 55)
(check-eq? (fib 11) 89)
(check-eq? (fib 12) 144)