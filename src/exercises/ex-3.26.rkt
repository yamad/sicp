#lang racket

;; Exercise 3.26 -- To search a table as implemented above, one needs
;; to scan through the list of records.  This is basically the
;; unordered list representation of section *Note 2-3-3.  For large
;; tables, it may be more efficient to structure the table in a
;; different manner.  Describe a table implementation where the (key,
;; value) records are organized using a binary tree, assuming that
;; keys can be ordered in some way (e.g., numerically or
;; alphabetically).  (Compare *Note Exercise 2-66 of *Note Chapter 2.)


;; binary tree representation
;; each node is (entry left-branch right-branch)
(define (make-tree entry left right)
  (mcons entry (mcons left right)))
(define (make-leaf entry)
  (make-tree entry '() '()))
(define entry mcar)
(define (left-branch tree)
  (mcar (mcdr tree)))
(define (right-branch tree)
  (mcdr (mcdr tree)))
(define set-entry! set-mcar!)
(define (set-left! root tree)
  (set-mcar! (mcdr root) tree))
(define (set-right! root tree)
  (set-mcdr! (mcdr root) tree))


;; construct table object, using predicate `same-key?` to compare keys
;; table is an n-dimensional table, with n-length keys, implemented
;; with binary trees.
(define (make-table same-key?)
  ;; a record is a key-value pair
  (define (make-record key value)
    (mcons key value))
  (define record-key mcar)
  (define record-value mcdr)
  (define (set-record-value! record value)
    (set-mcdr! record value))

  (define (entry-key db)
    (record-key (entry db)))
  (define (entry-value db)
    (record-value (entry db)))

  ;; the table is a record with the tree root as value
  (define table-key     record-key)
  (define table-records record-value)
  (define first-record  entry)

  (define (insert-record! record table)
    (set-mcdr! table (insert-record-tree! record (table-records table))))
  (define (insert-record-tree! record tree)
    (cond ((null? tree)
           (set! tree (make-leaf record)))
          ((= (record-key record) (entry-key tree))
           (set-entry! tree record))
          ((< (record-key record) (entry-key tree))
           (set-left! tree (insert-record-tree! record (left-branch tree))))
          ((> (record-key record) (entry-key tree))
           (set-right! tree (insert-record-tree! record (right-branch tree)))))
    tree)

  ;; return tree/leaf with key at root, false if no tree found
  (define (find-tree key tree)
    (cond ((null? tree) #f)
          ((= key (entry-key tree))
           tree)
          ((< key (entry-key tree))
           (find-tree key (left-branch tree)))
          ((> key (entry-key tree))
           (find-tree key (right-branch tree)))))

  ;; return record at key in table, false if no record found
  (define (find-record key table)
    (let ((tree (find-tree key (table-records table))))
      (if tree
          (entry tree)
          #f)))

  (let ((local-table (make-record '*table* '())))
    (define (print-table) local-table)

    (define (lookup keys)
      (define (lookup-record key table)
        (if table
            (find-record key table) #f))
      (let ((record
             (foldl lookup-record local-table keys)))
        (if record (record-value record) #f)))

    (define (insert! keys value)
      (define (find-or-create-record key table)
        (let ((record (find-record key table)))
          (cond (record record)
                (else
                 (let ((new (make-record key '())))
                   (insert-record! new table)
                   new)))))
      (set-record-value! (foldl find-or-create-record local-table keys) value)
      'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            ((eq? m 'print) print-table)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))


(require rackunit)
(require rackunit/text-ui)
(define tests
  (test-suite "n-dimensional table tests"
  (let ((table (make-table equal?)))
    (check-false ((table 'lookup) '(1)))
    ((table 'insert!) '(1) 'a)
    (check-equal? ((table 'lookup) '(1)) 'a)
    ((table 'insert!) '(2) 'b)
    (check-equal? ((table 'lookup) '(2)) 'b))
  (let ((table (make-table equal?)))
    ((table 'insert!) '(1 2) 'a)
    ((table 'insert!) '(1 3) 'b)
    ((table 'insert!) '(2 1) 'c)
    (check-equal? ((table 'lookup) '(1 2)) 'a)
    (check-equal? ((table 'lookup) '(1 3)) 'b)
    (check-equal? ((table 'lookup) '(2 1)) 'c))
  ))
(run-tests tests)
