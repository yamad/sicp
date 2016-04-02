#!/usr/bin/env mzscheme
#lang scheme

;; ex 1.9
;; ------

;; Procedure 1
(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))

;; Procedure 2
(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))


;; Evolution for P1
(+ 5 4)
(inc (+ (dec 5) 4))
(inc (inc (+ (dec 4) 4)))
(inc (inc (inc (+ (dec 3) 4))))
(inc (inc (inc (inc (+ (dec 2) 4)))))
(inc (inc (inc (inc (inc (+ (dec 1) 4))))))
(inc (inc (inc (inc (inc 4)))))
(inc (inc (inc (inc 5))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
9

;; Evolution for P2
(+ 5 4)
(+ (dec 5) (inc 4))
(+ (dec 4) (inc 5))
(+ (dec 3) (inc 6))
(+ (dec 2) (inc 7))
(+ (dec 1) (inc 8))
9