#lang racket

;; ex 2.74. Implement an integration strategy for employee records from different divisions

;; Implementers must specify in the dispatch table:
;;
;;   * (put <division-id> 'get-record <get-record-proc>) where
;;   `get-record-proc takes an 'employee and 'file and returns the
;;   employee record
;;
;;   * (put <division-id> 'get-salary <get-salary-proc>) where
;;   `get-salary-proc takes an employee record (untagged) and returns
;;   salary

;; get-record: retrieve employee record from file. The file should be
;; tagged with a division identifier (perhaps with a version), as the
;; first lineby itself.
;;
(define (get-record employee file)
  (define (get-div file)
    (car file))
  (make-tag (get-div file)
            ((get (get-div file) 'get-record)
             employee file)))

(define (make-tag tag data)
  (cons tag data))
(define get-tag car)
(define get-data cadr)

;; get-salary: retrieve salary from an (tagged) employee record
;; records will be tagged on retrieval.
(define (get-salary emp-rec)
  ((get (get-tag emp-rec) 'get-salary (get-data emp-rec))))

;; find-employee-record: retrieve employee record from a list of files
(define (find-employee-record employee lof)
  (if (null? lof)
      (error "find-employee-record: no record found")
      (let ((rec (get-record employee (car lof))))
        (cond ((null? rec) rec)
              (else (find-employee-record employee (cdr lof)))))))


;; d. New divisions, integrations, just need to obtain a unique
;; division id and then install get-record and get-salary functions
;; into the central dispatch table
