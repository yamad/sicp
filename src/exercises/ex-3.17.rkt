#lang racket
;; exercise 3.17 -- write a version of count-pairs that accounts for
;; repeated and/or recursive pairs

(define (count-pairs x)
  (define (count xs state)
    (let ((acc (car state))
          (seen (cdr state)))
      (if (or (not (pair? xs)) (member xs seen))
          state
          (count (cdr xs)
                 (count (car xs) (cons (+ acc 1) (cons xs seen)))))))
    (car (count x '(0 ()))))


(define (count-pairs-bad x)
  (if (not (pair? x))
      0
      (+ (count-pairs-bad (car x))
         (count-pairs-bad (cdr x))
         1)))

(define a
  (let* ((z (list 'z))
         (y (cons 'y z))
         (x (cons 'x y)))
    x))

(define b
  (let* ((z (list 'z))
         (y (cons z z))
         (x (cons 'x y)))
    x))

(define c
  (let* ((z (list 'z))
         (y (cons z z))
         (x (cons y y)))
    x))

(count-pairs a)
(count-pairs b)
(count-pairs c)

(count-pairs-bad a)
(count-pairs-bad b)
(count-pairs-bad c)
