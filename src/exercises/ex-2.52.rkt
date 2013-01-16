#lang racket
(require (planet soegaard/sicp:2:1/sicp))

(define (split orig-proc split-proc)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split orig-proc split-proc) painter (- n 1))))
          (orig-proc painter (split-proc smaller smaller))))))

(define right-split (split beside below))
(define up-split (split below beside))

;; b -- use one copy of up and right split
(define (corner-split-new painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1)))
            (corner (corner-split-new painter (- n 1))))
        (beside (below painter up)
                (below right corner)))))

;; c -- rearrange square-limit into different pattern
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((up-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter up-left)
                  (below bottom-right corner))))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-vert rotate180
                                  identity flip-horiz)))
    (combine4 (corner-split painter n))))

(paint (corner-split-new einstein 2))
(paint (square-limit einstein 2))
