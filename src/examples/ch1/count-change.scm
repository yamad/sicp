#!/usr/bin/env mzscheme
#lang scheme
;; SICP Section 1.2.2 (pg. 40)
;; Count Change

;; Problem: How many ways to change a particular amount, given
;; half-dollars, quarters, dimes, nickels, and pennies.

;; General Approach:
;; * order coins
;; * # of ways to make change =
;;     a) # of ways to change `a` excluding the first coin
;;     b) # of ways to change `a-d` using `n` coins, with `d` as denomination of first coin
;;
;; * edge cases
;;     a) if a = 0, 1 way to make change
;;     b) if a < 0, 0 ways to make change
;;     c) if n = 0, 0 ways to make change


(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount (- kinds-of-coins 1))
                 (cc (- amount (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination n)
  (cond ((= n 1) 1)
        ((= n 2) 5)
        ((= n 3) 10)
        ((= n 4) 25)
        ((= n 5) 50)))


(count-change 100)


;; Example: 10 cents, changed by dimes, nickels, and pennies
;; (cc 10 3)
;; (+ (cc 10 2) (cc 0 3))
;; (+ (+ (cc 10 1) (cc 5 2)) 1)
;; (+ (+ (cc 10 1) (+ (cc 5 1) (cc 0 2))) 1)
;; (+ (+ (cc 10 1) (+ (+ (cc 5 0) (cc 4 1)) 1)) 1)
;; (+ (+ (cc 10 1) (+ (+ 0 (+ 0 (cc 3 1))) 1)) 1)
;; (+ (+ (cc 10 1) (+ (+ 0 (+ 0 (+ 0 (cc 2 1)))) 1)) 1)
;; (+ (+ (cc 10 1) (+ (+ 0 (+ 0 (+ 0 (+ (cc 2 0) (cc 1 1))))) 1)) 1)
;; (+ (+ (cc 10 1) (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ (cc 1 0) (cc 0 1)))))) 1)) 1)
;; (+ (+ (cc 10 1) (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 1))))) 1)) 1)
;; (+ (cc 10 1) 3)
;; (+ (+ (cc 10 0) (cc 9 1)) 3)
;; (+ (+ 0 (+ (cc 9 0) (cc 8 1))) 3)
;; (+ (+ 0 (+ 0 (+ (cc 8 0) (cc 7 1)))) 3)
;; (+ (+ 0 (+ 0 (+ 0 (+ (cc 7 0) (cc 6 1))))) 3)
;; (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ (cc 6 0) (cc 5 1)))))) 3)
;; (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (cc 4 1)))))) 3)
;; (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ (cc 4 0) (cc 3 1))))))) 3)
;; (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (cc 2 1))))))) 3)
;; (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ (cc 2 0) (cc 1 1)))))))) 3)
;; (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ (cc 1 0) (cc 1 0))))))))) 3)
;; (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 1)))))))) 3)
;; 4