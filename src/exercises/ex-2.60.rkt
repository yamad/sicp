#lang racket

;; represent sets as lists with possible duplicates

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else
         (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (append set1 set2))))


(union-set '(1 2 3 4) '(3 4 5 6))
(intersection-set '(1 2 3 4) '(3 4 5 6))

;; Allowing duplicates in the set representation means that adjoin-set
;; and union-set take constant time since they do not have to check
;; the elements of the set beforehand. On the other hand,
;; element-of-set and intersection-set still have to check every
;; element of the set (or both sets). They "waste" time checking the
;; duplicate values.
;;
;; If the use case was mostly concerned with adjoin-set and union-set
;; operations, and rarely needed to test for set membership, then the
;; duplicate members representation might be appropriate.
