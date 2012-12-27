#lang racket

;; Interval Arithmetic

; original constructor
(define (make-interval a b)
  (if (< a b)
      (cons a b)
      (cons b a)))
(define (lower-bound z)
  (car z))
(define (upper-bound z)
  (cdr z))

;; center +- width constructor
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;; center +- percentage constructor
(define (make-center-percent c p)
  (let ((w (* c (/ p 100))))
    (make-center-width c w)))
(define (percent i)
  (* 100 (/ (width i) (abs (center i)))))
(define (percent-other i)
  (* 100 (/ (- (upper-bound i) (lower-bound i))
            (+ (upper-bound i) (lower-bound i)))))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (add-interval x
                (make-interval (- (upper-bound y))
                               (- (lower-bound y)))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (< (* (upper-bound y)
            (lower-bound y)) 0)
      (error "ERROR: The divisor cannot span 0")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(define (display-interval z)
  (newline)
  (display "(")
  (display (lower-bound z))
  (display " . ")
  (display (upper-bound z))
  (display ")"))

(define (display-center-percent z)
  (display "(")
  (display (center z))
  (display " +- ")
  (display (percent z))
  (display "%)")
  (newline))

;; test -- exercise 2.14
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define A (make-center-percent 100 2))
(define B (make-center-percent 50 0.5))
(define one (make-interval 1 1))

(display "par1: ")
(display-center-percent (par1 A B))

(display "par2: ")
(display-center-percent (par2 A B))

(display-center-percent (div-interval A B))
(display-center-percent (div-interval (div-interval A A) B))
(display-center-percent (div-interval one B))
(display-center-percent (div-interval A A))

;; exercise 2.15 and 2.16
; A/A is an identity (i.e, 1), but the interval arithmetic system does
; not know this and introduces the uncertainty into the measurement.
