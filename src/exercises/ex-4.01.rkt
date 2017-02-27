#lang racket
(require "../examples/metacircular-evaluator.rkt")

;; Exercise 4.1: Notice that we cannot tell whether the metacircular
;; evaluator evaluates operands from left to right or from right to
;; left. Its evaluation order is inherited from the underlying Lisp:
;; If the arguments to cons in `list-of-values' are evaluated from
;; left to right, then `list-of-values' will evaluate operands from
;; left to right; and if the arguments to cons are evaluated from
;; right to left, then `list-of-values' will evaluate operands from
;; right to left.

;; Write a version of `list-of-values' that evaluates operands from
;; left to right regardless of the order of evaluation in the
;; underlying Lisp.


;; "map force" over list of delayed comps while ensuring in-order eval
(define (eval-in-order xs)
  (if (null? xs)
      '()
      (cons (force (car xs))
            (eval-in-order (cdr xs)))))

;; evaluate expressions in left-to-right order
(define (list-of-values-ltr exps env)
  ;; build delayed list of eval calls
  (define (build xs)
    (if (no-operands? xs)
        '()
        (cons (delay (eval (first-operand xs) env))
              (build (rest-operands xs) env))))
  (eval-in-order (build exps)))


;; Also write a version of `list-of-values' that evaluates operands
;; from right to left.

(define (list-of-values-rtl exps env)
  ;; build delayed list of eval calls in reverse order
  (define (build xs acc)
    (if (no-operands? xs)
        acc
        (build (rest-operands xs)
              (cons
               (delay (eval (first-operand xs) env))
               acc))))
  (eval-in-order (build exps '())))



;; Alternative solution using nested let (from online)
(define (list-of-values-ltr-let exps env)
  (if (no-operands? exps)
      '()
      (let ((first
             (eval (first-operand exps) env)))
        (let ((rest
               (list-of-values-ltr-let (rest-operands exps) env)))
          (cons first rest)))))

(define (list-of-values-rtl-let exps env)
  (if (no-operands? exps)
      '()
      (let ((rest
             (list-of-values-rtl-let (rest-operands exps) env)))
        (let ((first
               (eval (first-operand exps) env)))
          (cons first rest)))))
