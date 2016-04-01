#lang racket

;; Exercise 3.24 -- In the table implementations above, the keys are
;; tested for equality using `equal?' (called by `assoc').  This is
;; not always the appropriate test.  For instance, we might have a
;; table with numeric keys in which we don't need an exact match to
;; the number we're looking up, but only a number within some
;; tolerance of it.  Design a table constructor `make-table' that
;; takes as an argument a `same-key?'  procedure that will be used to
;; test "equality" of keys.  `Make-table' should return a `dispatch'
;; procedure that can be used to access appropriate `lookup' and
;; `insert!' procedures for a local table.

;; fetch value at key in records, using predicate p to compare keys
(define (assocp key records p)
  (cond ((null? records) false)
        ((p key (mcar (mcar records))) (mcar records))
        (else (assocp key (mcdr records) p))))

;; construct table object, using predicate `same-key?` to compare keys
;;
;; only minor modifications of code given in text are required
(define (make-table same-key?)
  (let ((local-table (mcons '*table '())))
    (define (lookup key-1 key-2)
      (let ((subtable
             (assocp key-1 (mcdr local-table) same-key?)))
        (if subtable
            (let ((record
                   (assocp key-2 (mcdr subtable) same-key?)))
              (if record (mcdr record) false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable
             (assocp key-1 (mcdr local-table) same-key?)))
        (if subtable
            (let ((record
                   (assocp key-2 (mcdr subtable) same-key?)))
              (if record
                  (set-mcdr! record value)
                  (set-mcdr! subtable
                             (mcons (mcons key-2 value)
                                    (mcdr subtable)))))
            (set-mcdr! local-table
                       (mcons (mcons key-1 (mcons (mcons key-2 value) '()))
                              (mcdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))
