#lang planet neil/sicp

(define (split orig-proc split-proc)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split orig-proc split-proc) painter (- n 1))))
          (orig-proc painter (split-proc smaller smaller))))))

(define right-split (split beside below))
(define up-split (split below beside))


(paint (right-split einstein 2))
(paint (up-split einstein 2))
