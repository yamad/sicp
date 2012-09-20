#lang racket

(define dx 0.00001)
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (square x) (* x x))
(define (cube x) (* x x x))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? a b)
    (< (abs (- a b)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? next guess)
          next
          (try next))))
  (try first-guess))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (average a b)
  (/ (+ a b) 2))
(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (square-root-xform x)
  (fixed-point-of-transform
   (lambda (y) (/ x y))
   average-damp
   1.0))


;; ex 1.40
(define (cubic a b c)
  (lambda (x)
    (+ (cube x) (* a (square x)) (* b x) c)))

(newtons-method (cubic 1 2 1) 1.0)
