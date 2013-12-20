#lang racket

(require "getput.rkt")
(define op-table '())
(define (put op type item)
  (let ((key (make-key op type)))
    (set! op-table (cons (cons key item)
                         (prune-key key op-table)))))
(define (get op type)
  (let ((match (assoc (make-key op type) op-table)))
    (if match (cdr match) #f)))

;; ex-2.73
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
               var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;; a. expressions with operators are dispatched on the operator
;; type. numbers and variables are not operators, so the dispatch
;; can't work over these types.

;; b.
