#lang racket

;; exerice 2.58
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (multiplicand exp)
                        (deriv (multiplier exp) var))))
        ((exponentiation? exp)
         (make-product
          (make-product (exponent exp)
                        (make-exponentiation
                         (base exp) (- (exponent exp) 1)))
          (deriv (base exp) var)))
        (else
         (error "unkown expression type -- DERIV" exp))))


;; represent expressions in "standard" infix form
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1)
              (number? a2))
         (+ a1 a2))
        (else
         (list a1 '+ a2))))
(define (sum? x)
  (and (pair? x)
       (memq '+ x)))

; return expression before first '+
(define (addend s)
  (cond ((or (null? s) (eq? '+ (car s))) '())
        ((eq? '+ (cadr s)) (car s))
        (else
         (cons (car s) (addend (cdr s))))))

; return expression after first '+
(define (augend s)
  (let ((res (cdr (memq '+ s))))
    (if (null? (cdr res)) (car res) res)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else
         (list m1 '* m2))))
(define (product? x)
  (and (pair? x)
       (memq '* x)
       (not (memq '+ x))))
(define (multiplier p) (car p))
(define (multiplicand p)
  (if (null? (cdddr p))
      (caddr p)
      (cddr p)))

(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        ((and (number? b) (number? e))
         (expt b e))
        (else
         (list b '** e))))
(define (exponentiation? x)
  (and (pair? x)
       (memq '** x)))
(define base car)
(define exponent caddr)


(deriv '(x + 3) 'x)
(deriv '(x * y) 'x)
(deriv '((x * y) * (x + 3)) 'x)
(deriv '(x ** 3) 'x)
(deriv '(x + (3 * (x + (y + 2)))) 'x)
(deriv '(x + 3 * (x + y + 2)) 'x)
(deriv '(x + 3 * 6 * (x + y + 2)) 'x)
