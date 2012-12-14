#lang racket

;; Interval Arithmetic
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

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

;; ex 2.07
(define (make-interval a b)
  (cons a b))
(define (lower-bound z)
  (min (car z) (cdr z)))
(define (upper-bound z)
  (max (car z) (cdr z)))

;; ex 2.08
(define (sub-interval x y)
  (add-interval x
                (make-interval (- (upper-bound y))
                               (- (lower-bound y)))))

(define (display-interval z)
  (newline)
  (display "(")
  (display (lower-bound z))
  (display " . ")
  (display (upper-bound z))
  (display ")"))

(display-interval
 (sub-interval
  (make-interval 4 5)
  (make-interval 1 2)))

(display-interval
 (sub-interval
  (make-interval (- 4) (- 5))
  (make-interval 1 2)))

(display-interval
 (sub-interval
  (make-interval 4 5)
  (make-interval (- 1) (- 2))))
