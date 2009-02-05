;;EXERCISE 1.1
;: 10

;: (+ 5 3 4)

;: (- 9 1)

;: (/ 6 2)

;: (+ (* 2 4) (- 4 6))

;: (define a 3)

;: (define b (+ a 1))

;: (+ a b (* a b))

;: (= a b)

;: (if (and (> b a) (< b (* a b)))
;:     b
;:     a)

;: (cond ((= a 4) 6)
;:       ((= b 4) (+ 6 7 a))
;:       (else 25))

;: (+ 2 (if (> b a) b a))

;: (* (cond ((> a b) a)
;: 	 ((< a b) b)
;: 	 (else -1))
;:    (+ a 1))

;; ex 1.3
;;-------
;; a function that takes 3 arguments and finds the 
;; sum of squares for the two largest numbers

(define (square x) 
  (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (sum-of-squares-largest-two x y z)
  (if (>= x y)
    if (>= z y) (sum-of-squares x z) (sum-of-squares x y))
    if (>= z x) (sum-of-squares z y) (sum-of-squares x y)))

;; tests
(sum-of-squares-largest-two 1 2 3)
(sum-of-squares-largest-two 2 1 3)
(sum-of-squares-largest-two 3 1 2)
(sum-of-squares-largest-two 3 2 1)
;; result: 13


;;EXERCISE 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;;EXERCISE 1.5
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

;: (test 0 (p))


