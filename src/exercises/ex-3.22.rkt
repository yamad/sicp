#lang racket

;; exercise 3.22 -- instead of representing a queue as a pair of
;; pointers, we can build a queue as a procedure with local state. The
;; local state will consist of pointers to the beginning and the end
;; of an ordinary list. Thus, the make-queue procedure will have the
;; form [given]. Complete the definition of `make-queue` and provide
;; implementations of the queue operations using this implementation.
(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty?) (null? front-ptr))
    (define (front)
      (if (null? front-ptr)
          (error "Queue is empty")
          (mcar front-ptr)))
    (define (insert! item)
      (let ((new-item (mcons item '())))
        (cond ((empty?)
               (set! front-ptr new-item)
               (set! rear-ptr new-item))
              (else
               (set-mcdr! rear-ptr new-item)
               (set! rear-ptr new-item)))))
    (define (delete!)
      (cond ((empty?)
             (error "Queue is empty"))
            (else
             (set! front-ptr (mcdr front-ptr))
             dispatch)))
    (define (display-q)
      (display (list front-ptr rear-ptr))
      (newline))
    (define (dispatch m)
      (cond ((eq? m 'empty?) (empty?))
            ((eq? m 'front)  (front))
            ((eq? m 'insert!) insert!)
            ((eq? m 'delete!) (delete!))
            ((eq? m 'display) (display-q))
            (else
             (error "Unknown operation -- MAKE-QUEUE"))))
    dispatch))

(define (empty-queue? q) (q 'empty?))
(define (front-queue  q) (q 'front))
(define (insert-queue! q item) ((q 'insert!) item))
(define (delete-queue! q) (q 'delete!))
(define (display-queue q) (q 'display))


(define q1 (make-queue))
(insert-queue! q1 'a)
(display-queue q1)
(insert-queue! q1 'b)
(display (front-queue q1)) (newline)
(display-queue q1)
(delete-queue! q1)
(display-queue q1)
(delete-queue! q1)
(display-queue q1)
