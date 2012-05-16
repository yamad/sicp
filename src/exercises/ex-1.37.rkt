#lang racket

;; exercise 1.37
;; compute the value of k-term finite continued fraction

;; recursive version
(define (cont-frac-rec n d k)
  (define (recurse i)
    (if (> i k)
        0
        (/ (n i)
           (+ (d i)
              (recurse (+ i 1))))))
  (recurse 0))

;; iterative version
(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (zero? i)
        result
        (iter (- i 1)
              (/ (n i)
                 (+ (d i) result)))))
  (iter k 0))

(define k 12)

(define golden-ratio-approx-rec
  (/ 1
     (cont-frac-rec (lambda (i) 1.0)
                    (lambda (i) 1.0)
                    k)))

(define golden-ratio-approx-iter
  (/ 1
     (cont-frac-iter (lambda (i) 1.0)
                     (lambda (i) 1.0)
                     k)))
golden-ratio-approx-rec
golden-ratio-approx-iter


;; exercise 1.38
;; get D_i in the series 1,2,1,1,4,1,1,6,1,1,8,...
(define (euler-d i)
  (if (not (= (remainder (+ i 1) 3) 0))
      1
      (* 2 (/ (+ i 1) 3))))

(define (euler-d-convoluted i)
  (let ((norm-i (- i 2)))
    (cond ((< i 3) i)
          ((= (remainder norm-i 3) 0)
           (+ 2 (* 2 (/ norm-i 3))))
          (else 1))))

(define e
  (+ 2 (cont-frac-iter (lambda (i) 1.0) euler-d k)))
e