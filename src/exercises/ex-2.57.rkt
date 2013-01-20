#lang racket

;; exerice 2.57
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


;; represent expressions in "lisp" prefix form
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum . terms)
  (define (iter t acc)
    (cond ((null? t)
           (if (= acc 0) '()
               (list acc)))
          ((=number? (car t) 0)
           (iter (cdr t) acc))
          ((number? (car t))
           (iter (cdr t) (+ (car t) acc)))
          (else
           (cons (car t) (iter (cdr t) acc)))))
  (let ((res (iter terms 0)))
    (if (= (length res) 1)
        (car res)
        (cons '+ res))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (apply make-sum (cddr s)))

(define (make-product . terms)
  (define (iter t acc new-terms)
    (cond ((null? t)
           (cond ((null? new-terms) (list acc))
                 ((= acc 1) new-terms)
                 (else (cons acc new-terms))))
          ((=number? (car t) 0)
           (list 0))
          ((=number? (car t) 1)
           (iter (cdr t) acc new-terms))
          ((number? (car t))
           (iter (cdr t) (* acc (car t)) new-terms))
          (else
           (iter (cdr t) acc (cons (car t) new-terms)))))
  (let ((res (iter terms 1 '())))
    (if (= (length res) 1)
        (car res)
        (cons '* res))))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (apply make-product (cddr p)))

(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        ((and (number? b) (number? e))
         (expt b e))
        (else
         (list '** b e))))
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))
(define base cadr)
(define exponent caddr)


(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* x y (+ x 3)) 'x)
(deriv '(** x 3) 'x)

(make-sum 2 4 5)
(make-sum 'a 'b 2 4 5 'a 'b)

(make-product 0 2 4)
(make-product 1)
(make-product 'a 'b 0 'c)
(make-product 1 'a 'b)
