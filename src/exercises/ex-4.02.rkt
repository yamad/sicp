#lang racket
(require "../examples/metacircular-evaluator.rkt")

;; Exercise 4.2: Louis Reasoner plans to reorder the cond clauses in eval
;; so that the clause for procedure applications appears before the
;; clause for assignments. He argues that this will make the interpreter
;; more efficient: Since programs usually contain more applications than
;; assignments, definitions, and so on, his modiﬁed eval will usually
;; check fewer clauses than the original eval before identifying the type
;; of an expression.

;; a. What is wrong with Louis’s plan? (Hint: What will Louis’s evaluator
;; do with the expression (define x 3)?)

; If the procedure application clauses comes first (or early), then it
; will attempt to apply any special forms (e.g. define, quote, if,
; lambda) as a procedure to arguments. Thus, all special forms need to
; be identified first before procedure application is assumed.

;; b. Louis is upset that his plan didn’t work. He is willing to go to
;; any lengths to make his evaluator recognize procedure applications
;; before it checks for most other kinds of expressions. Help him by
;; changing the syntax of the evaluated language so that procedure
;; applications start with call. For example, instead of (factorial 3) we
;; will now have to write (call factorial 3) and instead of (+ 1 2) we
;; will have to write (call + 1 2).


;; Changing the syntax is easy
;; procedure application as (call <proc> <args>)
(define (application? exp) (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))

;; then we just move the application? clause earlier in eval
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        (else
         (error "Unknown expression type: EVAL" exp))))
