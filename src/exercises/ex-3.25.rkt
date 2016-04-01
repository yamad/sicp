#lang racket

;; Exercise 3.25 -- Generalizing one- and two-dimensional tables, show
;; how to implement a table in which values are stored under an
;; arbitrary number of keys and different values may be stored under
;; different numbers of keys.  The `lookup' and `insert!' procedures
;; should take as input a list of keys used to access the table.

;; fetch value at key in records, using predicate p to compare keys
(define (assocp key records p)
  (cond ((null? records) false)
        ((p key (mcar (mcar records))) (mcar records))
        (else (assocp key (mcdr records) p))))

;; construct table object, using predicate `same-key?` to compare keys
;;
;; only minor modifications of code given in text are required
(define (make-table same-key?)
  (let ((local-table (mcons '*table* '())))
    (define (table? a)
      (and (mpair? a) (eq? '*table* (mcar a))))

    (define (print-table) local-table)

    (define (lookup keys)
      (define (lookup-table keys table)
        (cond ((null? keys)  false)
              ((null? table) false)
              (else
               (let ((record
                      (assocp (car keys) (mcdr table) same-key?)))
                 (cond ((not record) false)
                       ((table? (mcdr record))
                        (lookup-table (cdr keys) (mcdr record)))
                       (else (mcdr record)))))))
      (lookup-table keys local-table))

    (define (insert! keys value)
      (insert-table! keys value local-table))

    (define (insert-table! keys value table)
      (cond ((null? keys) value)
            (else
             (let ((record (assocp (car keys) (mcdr table) same-key?))
                   (new-table (make-table same-key?)))
               (cond ((and record (table? (mcdr record))) ; existing key/table
                      (set-mcdr! record
                                 (insert-table! (cdr keys) value (mcdr record))))
                     (record                              ; existing key, new table
                      (set-mcdr! record
                                 ((new-table 'insert-proc!) (cdr keys) value)))
                     (else                                ; set new key-value record
                      (set-mcdr! table
                                 (mcons
                                  (mcons (car keys)
                                         ((new-table 'insert-proc!) (cdr keys) value))
                                  (mcdr table)))))
               table))))

    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            ((eq? m 'print) print-table)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))


(define a (make-table equal?))
((a 'insert-proc!) '(a b c) 1)
((a 'insert-proc!) '(a b d) 2)
((a 'insert-proc!) '(a c d) 3)

((a 'lookup-proc) '(a b c))
((a 'lookup-proc) '(a b d))
((a 'lookup-proc) '(a c d))
