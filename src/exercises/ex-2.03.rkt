#lang racket

;; ex 2.03

;; segments
(define (make-segment pa pb)
  (cons pa pb))
(define (start-segment s)
  (car s))
(define (end-segment s)
  (cdr s))

;; points
(define (make-point a b)
  (cons a b))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))


;; rectangle, implementation A
;; pa and pb, are the top left and bottom right points, respectively
(define (make-rect-a pa pb)
  (cons pa pb))
(define (rect-a-tl r)
  (car r))
(define (rect-a-br r)
  (cdr r))

(define (rect-a-height r)
  (rect-distance r y-point))

(define (rect-a-base r)
  (rect-distance r x-point))

(define (rect-distance r coord)
  (abs (- (coord (rect-a-tl r))
          (coord (rect-a-br r)))))

(define (rect-a-perimeter r)
  (+ (* 2 (rect-a-height r))
     (* 2 (rect-a-base r))))

(define (rect-a-area r)
  (* (rect-a-base r)
     (rect-a-height r)))


(define rect-a-a
  (make-rect-a (make-point 0 5)
               (make-point 2 0)))

(rect-a-height rect-a-a)
(rect-a-base rect-a-a)
(rect-a-perimeter rect-a-a)
(rect-a-area rect-a-a)


;; rectangle, implementation B
;; defined by center point, base (b) and height (h)
(define (make-rect-b center b h)
  (cons center (cons b h)))

(define (rect-b-center r)
  (car r))

(define (rect-b-base r)
  (car (cdr r)))
(define (rect-b-height r)
  (cdr (cdr r)))

;; note that perimeter and area are defined identically as in
;; implementation A, just with slightly different names to avoid
;; multiple definition errors
(define (rect-b-perimeter r)
  (+ (* 2 (rect-b-height r))
     (* 2 (rect-b-base r))))

(define (rect-b-area r)
  (* (rect-b-base r)
     (rect-b-height r)))

(define rect-b-a
  (make-rect-b (make-point 3 5)
               6 10))

(rect-b-height rect-b-a)
(rect-b-base rect-b-a)
(rect-b-perimeter rect-b-a)
(rect-b-area rect-b-a)
