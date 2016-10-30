#lang racket
;; recreate SICP streams, which work differently from Racket streams

;; cons-stream and delay must be 'special forms' so that their
;; arguments are not evaluated immediately. instead, the functions are
;; defined as macros so that their arguments pass through unevaluated
;; until needed. the macro can be thought of as simple text
;; substitution.

;; stream data type, lazy evaluation
(define-syntax-rule (cons-stream a b)
  (cons a (delay b)))
(define the-empty-stream '())
(define stream-null? null?)
(define (stream-car s)
  (if (stream-null? s)
      the-empty-stream
      (car s)))
(define (stream-cdr s)
  (if (stream-null? s)
      the-empty-stream
      (force (cdr s))))

;; underlying lazy evaluation
(define-syntax-rule (delay proc)
  (memo-proc (lambda () proc)))
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

;; for some exercises
(define-syntax-rule (delay-no-memo proc)
  (lambda () proc))
(define-syntax-rule (cons-stream-no-memo a b)
  (cons a (delay-no-memo b)))

;; stream operations
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter
                       pred
                       (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (stream-foldl proc stream init)
  (if (stream-null? stream)
      init
      (stream-foldl proc (stream-cdr stream)
                    (proc (stream-car stream) init))))

(define (stream-foldr proc init stream)
  (if (stream-null? stream) init
      (proc (stream-car stream)
            (stream-foldr proc init (stream-cdr stream)))))

(define (display-stream s)
  (stream-for-each displayln s))

(define (take n s)
  (cond ((= n 0)          the-empty-stream)
        ((stream-null? s) the-empty-stream)
        (else
         (cons-stream (stream-car s)
                      (take (- n 1) (stream-cdr s))))))

(define (take-as-list n s)
  (stream-foldr cons '() (take n s)))

;; useful stream constructors
;; --------------------------
(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (add-streams a b) (stream-map + a b))

(define (mul-streams a b) (stream-map * a b))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (repeat x)
  (cons-stream x (repeat x)))

(define (negate-stream s)
  (scale-stream s -1))

(define (list-to-stream l)
  (if (null? l)
      the-empty-stream
      (cons-stream (car l) (list-to-stream (cdr l)))))

;; defined streams
;; ---------------
(define ones (cons-stream 1 ones))
(define integers
  (cons-stream 1 (add-streams ones integers)))

;; sieve of Eratosthenes
(define (divisible? x y) (= (remainder x y) 0))
(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))
(define primes (sieve (integers-starting-from 2)))

(define (fibgen a b) (cons-stream a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))

;; 3.5.3 exploiting the stream paradigm
(define (average x y)
  (/ (+ x y) 2))
(define (sqrt-improve guess x)
  (average guess (/ x guess)))
(define (square x) (* x x))

(define (sqrt-stream x)
  (define guesses
    (cons-stream
     1.0
     (stream-map (lambda (guess) (sqrt-improve guess x))
                 guesses)))
  guesses)

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))

(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream
     initial-value
     (let ((integrand (force delayed-integrand)))
       (add-streams (scale-stream integrand dt)
                    int))))
  int)

;; generate stream from list of streams
;; e.g.
;;   (list-of-streams integers integers)
;;    --> (1 1) (2 2) (3 3) ...
;;
;;   (list-of-streams integers integers integers)
;;    --> (1 1 1) (2 2 2) (3 3 3) ...
;;
(define (list-of-streams . streams)
  (cons-stream (map stream-car streams)
               (apply list-of-streams (map stream-cdr streams))))

(provide cons-stream
         cons-stream-no-memo
         stream-null?
         the-empty-stream
         stream-car
         stream-cdr
         stream-ref
         stream-map
         stream-for-each
         stream-foldr
         stream-filter
         display-stream
         take
         take-as-list
         stream-enumerate-interval
         scale-stream
         negate-stream
         add-streams
         mul-streams
         ones
         integers
         repeat
         sqrt-stream
         make-tableau
         euler-transform
         accelerated-sequence
         list-to-stream
         integral
         list-of-streams
         delay
         force
         )
