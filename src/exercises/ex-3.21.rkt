#lang racket

;; The 'b' atom that gets printed in the example is the same object in
;; internal memory. It is just referenced twice.
;;
;; Each element in the queue is represented once (one location in
;; memory). However, the last element is _referenced_ twice. The
;; default print routine for pairs prints the contents of the car and
;; cdr. In the underlying representation of the queue, the car is the
;; list of all elements and cdr is the pointer to the last element of
;; that list. Given this representation, it is enough to just print
;; the car of the queue.

(define (print-queue queue)
  (display (car queue)))
