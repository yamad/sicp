#lang racket

;; Exercise 3.25 -- Generalizing one- and two-dimensional tables, show
;; how to implement a table in which values are stored under an
;; arbitrary number of keys and different values may be stored under
;; different numbers of keys.  The `lookup' and `insert!' procedures
;; should take as input a list of keys used to access the table.

;; construct table object, using predicate `same-key?` to compare keys
;; table is an n-dimensional table, with n-length keys.
(define (make-table same-key?)
  (let ((local-table (mcons '*table* '())))
    ;; a table of records is a header record with a record-list as
    ;; value: (key record-list). a record-list is a backbone-linked
    ;; set of records, with each backbone link as (record next-record)
    ;;
    ;;               table                record list
    ;;               [<table key>][.]--->[.][.]--->[.][/]
    ;;                             |         |
    ;; record pairs              [k1][v1]  [k2][v2]
    ;;
    ;; a record is a key-value pair
    (define (make-record key value)
      (mcons key value))
    (define (insert-record! record table)
      (set-mcdr! table (mcons record (mcdr table)))
      table)
    (define (table-key table) (mcar table))
    (define (table-records table) (mcdr table))
    (define (first-record record-list) (mcar record-list))
    (define (rest-records record-list) (mcdr record-list))
    (define (record-key record) (mcar record))
    (define (record-value record) (mcdr record))
    ;; return record at key in table, false if no record found
    (define (find-record key table)
      (assocp key (table-records table)))
    (define (assocp key records)
      (cond ((null? records) false)
            ((same-key? key (record-key (first-record records)))
             (first-record records))
            (else (assocp key (rest-records records)))))

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
                 (insert-record! (make-record key '()) table)
                 (first-record (table-records table))))))
      (set-mcdr! (foldl find-or-create-record local-table keys) value)
      'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            ((eq? m 'print) print-table)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))


(require rackunit)
(require rackunit/text-ui)
(run-tests
 (test-suite "n-dimensional table tests"
  (let ((table (make-table equal?)))
    (check-false ((table 'lookup) '(a)))
    (check-equal? (begin ((table 'insert!) '(a) 1) ((table 'print)))
                  (mcons '*table*
                         (mcons (mcons 'a 1) '())))
    (check-equal? (begin ((table 'insert!) '(b) 2) ((table 'print)))
                  (mcons '*table*
                         (mcons (mcons 'b 2)
                                (mcons (mcons 'a 1) '())))))
  (let ((table (make-table equal?)))
    (check-equal? (begin ((table 'insert!) '(a b) 1) ((table 'print)))
                  (mcons '*table*
                         (mcons (mcons 'a
                                       (mcons (mcons 'b 1) '())) '())))
    (check-equal? (begin ((table 'insert!) '(a c) 2) ((table 'print)))
                 (mcons '*table*
                        (mcons (mcons 'a
                                      (mcons (mcons 'c 2)
                                             (mcons (mcons 'b 1) '())))
                               '())))
    (check-equal? ((table 'lookup) '(a b)) 1)
    (check-equal? ((table 'lookup) '(a c)) 2))
  (let ((table (make-table equal?)))
    (begin
      ((table 'insert!) '(a b c) 1)
      ((table 'insert!) '(x b c) 2)
      ((table 'insert!) '(a b x) 3)
      (check-equal? ((table 'lookup) '(a b c)) 1)
      (check-equal? ((table 'lookup) '(x b c)) 2)
      (check-equal? ((table 'lookup) '(a b x)) 3)))
  ))
