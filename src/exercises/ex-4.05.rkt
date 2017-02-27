#lang racket
(require (except-in "../examples/metacircular-evaluator.rkt" expand-clauses eval))
(require "ex-4.03.rkt")                 ; modular eval

;; Exercise 4.5: Scheme allows an additional syntax for `cond'
;; clauses, `(<TEST> => <RECIPIENT>)'.  If <TEST> evaluates to a true
;; value, then <RECIPIENT> is evaluated.  Its value must be a
;; procedure of one argument; this procedure is then invoked on the
;; value of the <TEST>, and the result is returned as the value of the
;; `cond' expression.  For example
;;
;;       (cond ((assoc 'b '((a 1) (b 2))) => cadr)
;;             (else false))
;;
;; returns 2.  Modify the handling of `cond' so that it supports this
;; extended syntax.

(define (cond-arrow? clause)
  (eq? (and (list? clause)
            (cadr clause))
       '=>))
(define (cond-arrow-test clause) (car clause))
(define (cond-arrow-recipient clause) (caddr clause))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                            ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (cond ((cond-else-clause? first)
               (if (null? rest)
                   (sequence->exp (cond-actions first))
                   (error "ELSE clause isn't last: COND->IF"
                          clauses)))
               ((cond-arrow? first)
                (make-if (cond-arrow-test first)
                         (list (cond-arrow-recipient first)
                               (cond-arrow-test first))
                         'false))
               (else
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest)))))))

(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (eval-cond exp env)
  (eval (cond->if exp) env))
(put 'cond eval-cond)
