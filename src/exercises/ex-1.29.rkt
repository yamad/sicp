#lang racket

;; exercise 1.29
;; My implementation of Simpson's Rule for approximating the integral.
;; Redefines the sum operation so that it keeps track of the iteration
;; index as well. The increment directly increments the value of x by
;; the step size determined by n. The function f is then simply
;; applied directly to x. A bit more "clever" in some ways, but
;; probably less clear than the solution below this one.
(define (cube x) (* x x x))
(define (ensure-even x)
  (if (even? x) x
      (+ x 1)))
(define (sum-idx term a next b i)
  (if (> a b)
      0
      (+ (term a i)
         (sum-idx term (next a) next b (+ i 1)))))

;; Simpson's rule
(define (simpson-integral f a b n)
  (define fixed-n (ensure-even n))
  (define h (/ (- b a) fixed-n))
  (define (simp-term x i)
    (* (f x)
       (cond ((or (= i 0) (= i fixed-n)) 1.0)
             ((even? i) 2.0)
             (else 4.0))))
  (define (simp-inc x)
    (+ x h))
  (* (/ h 3.0)
     (sum-idx simp-term a simp-inc b 0)))

(simpson-integral cube 0 1 100)
(simpson-integral cube 0 1 1003)


;; After looking at some online solutions, most others seemed to take
;; the approach of using the summation abstraction as a simple loop
;; where a starts at 0 and b is the number of steps. This approach is
;; a little cleaner, in part because the sum procedure doesn't need to
;; be redefined. The two implementations seem to produce identical
;; results.
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (simpson-integral-other f a b n)
  (define fixed-n (ensure-even n))
  (define h (/ (- b a) fixed-n))
  (define (yk k) (f (+ a (* k h))))
  (define (inc x) (+ x 1))
  (define (simpson-term k)
    (* (yk k)
       (cond ((or (= k 0) (= k fixed-n)) 1.0)
             ((even? k) 2.0)
             (else 4.0))))
  (* (/ h 3.0)
     (sum simpson-term 0 inc fixed-n)))

(simpson-integral-other cube 0 1 100)
(simpson-integral-other cube 0 1 1003)
