#lang racket

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define (make-mutex)
  (let ((cell (mcons false '())))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (when (test-and-set! cell)
               (the-mutex 'acquire)))   ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell)
  (set-mcar! cell false))

(require ffi/unsafe/atomic)
(define (test-and-set! cell)
  (call-as-atomic
   (lambda ()
     (if (mcar cell)
         true
         (begin (set-mcar! cell true) false)))))


(provide make-serializer
         make-mutex
         clear!
         test-and-set!)
