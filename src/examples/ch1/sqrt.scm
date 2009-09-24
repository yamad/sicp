;;; SICP section 1.1.7
;;; Newton's Method to Approximate Square Roots

;; Improve guesses until one is "good enough"
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

;; An improvement of the guess -- (guess + (radicand / guess)) / 2
(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

;; A bad implementation of `good-enough?` that tests for similarity
;; within a small tolerance
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (square x) (* x x))

;; Always use 1.0 as an initial arbitrary guess
(define (sqrt x)
  (sqrt-iter 1.0 x))



;; Tests

;; import schemeunit
(require (planet schematics/schemeunit))

(test-case
 "tests" (
 (check-eqv?
  (sqrt 9)
  3)

 (check-eq?
  (sqrt (+ 100 37))
  11.704699917758145)

 (check-eq?
  (sqrt (+ (sqrt 2) (sqrt 3)))
  1.7739279023207892)

 (check-eq?
  (square (sqrt 1000))
  1000.000369924366)
 ))
(run-tests 'tests')