#lang racket

;; Convert binary tree to a list (two implementations)
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

;; binary trees
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))


;; testing
(define (make-leaf entry)
  (make-tree entry '() '()))


(define a (make-tree 7
                     (make-tree 3
                                (make-leaf 1)
                                (make-leaf 5))
                     (make-tree 9
                                '()
                                (make-leaf 11))))

(define b (make-tree 3
                     (make-leaf 1)
                     (make-tree 7
                                (make-leaf 5)
                                (make-tree 9
                                           '()
                                           (make-leaf 11)))))

(define c (make-tree 5
                     (make-tree 3
                                (make-leaf 1)
                                '())
                     (make-tree 9
                                (make-leaf 7)
                                (make-leaf 11))))

(tree->list-1 a)
(tree->list-2 a)

(tree->list-1 b)
(tree->list-2 b)

(tree->list-1 c)
(tree->list-2 c)


;; a. These are recursive and iterative versions of the identical
;; algorithm, and so should return the same result for any input. The
;; difference is only that the iterative version accumulates the
;; result in the result-list argument.
;;
;; b. The number of calls to each procedure is the same, so the order
;; of growth for the function calls themselves in O(n). Other
;; solutions, however, note that the append function in the first
;; version also takes O(n) time for its input lists. In this case, the
;; input lists are n/2 on each call if the tree is balanced. Therefore
;; the total order of growth for the first implementation is O(n log
;; n), compared to O(n) for the second implementation.
