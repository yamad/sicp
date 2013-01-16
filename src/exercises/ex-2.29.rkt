#lang racket

;; exercise 2.29
(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))

(define left-branch car)
(define right-branch cadr)

(define branch-length car)
(define branch-structure cadr)

(define mobile? list?)

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define (mobile-balanced? mobile)
  (let ((l (left-branch mobile))
        (r (right-branch mobile)))
    (and
     (eq? (branch-torque l)
          (branch-torque r))
     (branch-balanced? l)
     (branch-balanced? r))))

(define (branch-weight branch)
  (let ((bs (branch-structure branch)))
    (if (mobile? bs)
        (total-weight bs)
        bs)))
(define (branch-torque branch)
  (* (branch-length branch)
     (branch-weight branch)))
(define (branch-balanced? branch)
  (let ((bs (branch-structure branch)))
    (if (mobile? bs)
        (mobile-balanced? bs)
        #t)))


(define a (make-mobile (make-branch 2 3)
                       (make-branch 2 3)))
(define b (make-mobile (make-branch 2 3)
                       (make-branch 4 5)))
(define c (make-mobile (make-branch 5 a)
                       (make-branch 3 b)))
(define d (make-mobile (make-branch 10 a)
                       (make-branch 12 5)))


(total-weight a) "-->" 6
(total-weight b) "-->" 8
(total-weight c) "-->" 14
(total-weight d) "-->" 11

(mobile-balanced? a)
(mobile-balanced? b)
(mobile-balanced? c)
(mobile-balanced? d)


;; if constructors were changed to be:
;
;; (define (make-mobile left right)
;;   (cons left right))
;; (define (make-branch length structure)
;;   (cons length structure))

;; then only the following changes are needed:
;

;; (define right-branch cdr)
;; (define branch-structure cdr)
