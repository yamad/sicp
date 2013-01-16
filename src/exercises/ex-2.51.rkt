#lang racket
(require (planet soegaard/sicp:2:1/sicp))

(define (below-jyh painter1 painter2)
  (let ((split-point (make-vect 0 0.5)))
    (let ((paint-top
           ((transform-painter (make-vect 0 0.5)
                               (make-vect 1.0 0.5)
                               (make-vect 0 1.0))
            painter2))
          (paint-bottom
           ((transform-painter (make-vect 0 0)
                               (make-vect 1.0 0)
                               (make-vect 0 0.5))
            painter1)))
      (lambda (frame)
        (paint-top frame)
        (paint-bottom frame)))))

(define (below-jyh2 painter1 painter2)
  (rotate90 (beside painter1 painter2)))

(paint (below-jyh gray black))
(paint (below-jyh2 gray black))
