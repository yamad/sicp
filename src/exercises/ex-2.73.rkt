#lang racket

(require "getput.rkt")
(define op-table '())
(define (put op type item)
  (let ((key (make-key op type)))
    (set! op-table (cons (cons key item)
                         (prune-key key op-table)))))
(define (get op type)
  (let ((match (assoc (make-key op type) op-table)))
    (if match (cdr match)
        (error "get error: no entry for " op type))))

;; ex-2.73
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
               (operands exp) var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num)
  (and (number? exp) (= exp num)))

;; a. expressions with operators are dispatched on the operator
;; type. numbers and variables are not operators, so the dispatch
;; can't work over these types.

;; b. procedures for sums and products
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1)
              (number? a2))
         (+ a1 a2))
        (else
         (list '+ a1 a2))))
(put 'deriv '+
     (lambda (ops var)
       (make-sum (deriv (car ops) var)
                 (deriv (cadr ops) var))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else
         (list '* m1 m2))))
(put 'deriv '*
     (lambda (ops var)
       (make-sum (make-product
                  (car ops)
                  (deriv (cadr ops) var))
                 (make-product
                  (deriv (car ops) var)
                  (cadr ops)))))

;; c. implement exponentiation differentiation rule
(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        ((and (number? b) (number? e))
         (expt b e))
        (else
         (list '** b e))))
(put 'deriv '**
     (lambda (ops var)
       (make-product
        (make-product (cadr ops)
                      (make-exponentiation
                       (car ops) (- (cadr ops) 1)))
        (deriv (car ops) var))))

;; tests
(deriv '(+ x 3) 'x)
(deriv '(+ x (+ x 3)) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)
(deriv '(** x 3) 'x)


;; d. just switch the arguments in the 'put call
