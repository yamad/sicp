#lang racket

(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

(define make-vect cons)
(define xcor-vect car)
(define ycor-vect cdr)

(define (add-vect a b)
  (make-vect (+ (xcor-vect a)
                (xcor-vect b))
             (= (ycor-vect a)
                (ycor-vect b))))

(define (sub-vect a b)
  (make-vect (- (xcor-vect a)
                (xcor-vect b))
             (- (ycor-vect a)
                (ycor-vect b))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))
