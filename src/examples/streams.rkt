#lang racket
;; recreate SICP streams, which work differently from Racket streams

;; cons-stream and delay must be 'special forms' so that their
;; arguments are not evaluated immediately. instead, the functions are
;; defined as macros so that their arguments pass through unevaluated
;; until needed. the macro can be thought of as simple text
;; substitution.
(define-syntax-rule (cons-stream a b)
  (cons a (delay b)))
(define-syntax-rule (delay proc)
  (memo-proc (lambda () proc)))
(define the-empty-stream '())
(define (stream-car s) (car s))
(define (stream-cdr s) (force (cdr s)))
(define stream-null? null?)

(define (force obj) (obj))
(define (memo-proc proc)
  (let ((already-run? false)
        (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (display-stream s)
  (stream-for-each displayln s))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter
                       pred
                       (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(provide (all-defined-out))
