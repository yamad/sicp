#lang racket

;; define union-set and intersection-set as O(n) procedures to generate balanced trees
(define (union-set set1 set2)
  (let ((x1 (tree->list set1))
        (x2 (tree->list set2)))
    (list->tree (union-set-olist x1 x2))))

(define (intersection-set set1 set2)
  (let ((x1 (tree->list set1))
        (x2 (tree->list set2)))
    (list->tree (intersection-set-olist x1 x2))))

;; convert ordered list to balanced tree, O(n)
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

;; convert tree to an ordered-list, iterative version, O(n)
(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

;; binary tree representation
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

;; (ex-2.62) ordered-list representation, O(n)
(define (union-set-olist set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1))
               (x2 (car set2)))
           (cond ((= x1 x2)
                  (cons x1 (union-set-olist (cdr set1)
                                      (cdr set2))))
                 ((< x1 x2)
                  (cons x1 (union-set-olist (cdr set1)
                                      set2)))
                 ((> x1 x2)
                  (cons x2 (union-set-olist set1
                                      (cdr set2)))))))))

;; (ex-2.61) ordered-list representation, O(n)
(define (intersection-set-olist set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set-olist (cdr set1)
                                          (cdr set2))))
              ((< x1 x2)
               (intersection-set-olist (cdr set1) set2))
              ((> x1 x2)
               (intersection-set-olist set1 (cdr set2)))))))



(define a (list->tree '(1 3 5 7 9 11)))
(define b (list->tree '(2 4 5 6 9)))

a
b
(union-set a b)
(intersection-set a b)
