#lang racket
(require (except-in "../examples/metacircular-evaluator.rkt" eval))

;; Exercise 4.3: Rewrite eval so that the dispatch is done in
;; data-directed style. Compare this with the data-directed
;; differentiation procedure of Exercise 2.73. (You may use the car of a
;; compound expression as the type of the expression, as is appropriate
;; for the syntax implemented in this section.)

;; need a place to store procedures
(define dispatch-table '())

(define (prune-key key table)
  (define (remove-kv key table)
    (cond ((null? table) table)
          ((equal? key (caar table))
           (cdr table))
          (else
           (cons (car table) (remove-kv key (cdr table))))))
  (if (member key (map car table))
      (remove-kv key table)
      table))

(define (put key item)
  (set! dispatch-table (cons (cons key item)
                             (prune-key key dispatch-table))))
(define (get key)
  (let ((match (assoc key dispatch-table)))
    (if match (cdr match)
        #f)))

;; define dispatch functions, each taking an expression and the
;; environment, from the original eval procedure
(define (eval-quote exp env)
  (text-of-quotation exp))
(define (make-lambda exp env)
  (make-procedure (lambda-parameters exp)
                  (lambda-body exp)
                  env))
(define (eval-begin exp env)
  (eval-sequence (begin-actions exp) env))
(define (eval-cond exp env)
  (eval (cond->if exp) env))

;; install dispatch functions keyed by syntactic tag
(put 'quote eval-quote)
(put 'set! eval-assignment)
(put 'define eval-definition)
(put 'if eval-if)
(put 'lambda make-lambda)
(put 'begin eval-begin)
(put 'cond eval-cond)

(define (eval exp env)
  (let ((proc (and (pair? exp) (get (car exp)))))
    (cond ((self-evaluating? exp) exp)  ; untyped arguments
          ((variable? exp) (lookup-variable-value exp env))
          (proc (proc exp env))         ;
          ((application? exp)           ; if we get here, unknown tag
           (apply (eval (operator exp) env)
                  (list-of-values (operands exp) env)))
          (else
           (error "Unknown expression type: EVAL" exp)))))

(provide put
         get
         eval)
