#lang racket

;; lookup procedure for a database modeled as a binary tree
;;
;; get the record identified by the given-key from the database db
(define (lookup given-key db)
  (cond ((null? db) false)
        ((= given-key (entry-key db))
         (entry db))
        ((< given-key (entry-key db))
         (lookup given-key (left-branch db)))
        ((> given-key (entry-key db))
         (lookup given-key (right-branch db)))))

;; records are stored in the entry of the tree node
;; as a list (key payload)
(define (make-record key value)
  (list key value))
(define key car)
(define value cadr)

(define (entry-key db)
  (key (entry db)))
(define (entry-value db)
  (value (entry db)))

;; binary tree representation
;; each node is (entry left-branch right-branch)
(define (make-tree entry left right)
  (list entry left right))
(define (make-leaf entry)
  (make-tree entry '() '()))
(define entry car)
(define left-branch cadr)
(define right-branch caddr)
