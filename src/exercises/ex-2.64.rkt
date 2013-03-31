#lang racket

;; explain how partial-tree works

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


;; partial-tree takes a list of elements and produces a balanced tree
;; from the first n members. To do this, it takes the first half of
;; the list (calculating the number of elements to take) and
;; recursively generates a balanced tree from these elements. It then
;; takes all but the first of the leftover elements and generates a
;; balanced tree from these elements. It then places the first of the
;; leftover elements at the root of a new tree composed of the left
;; and right balanced trees.

;; the order of growth is O(n), because it must visit every element of
;; the list.

;; binary trees
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

;; testing
(define (make-leaf entry)
  (make-tree entry '() '()))


(list->tree '(1 3 5 7 9 11))
;; should produce
(make-tree 5
           (make-tree 1
                      '()
                      (make-leaf 3))
           (make-tree 9
                      (make-leaf 7)
                      (make-leaf 11)))
