#lang racket
(require "../examples/serializer.rkt")
;; Exercise 3.48: Explain in detail why the deadlock-avoidance method
;; described above, (i.e., the accounts are numbered, and each process
;; attempts to acquire the smaller-numbered account first) avoids
;; deadlock in the exchange problem. Rewrite serialized-exchange to
;; incorporate this idea. (You will also need to modify make-account
;; so that each account is created with a number, which can be
;; accessed by sending an appropriate message.)
;;
;;
;; By insisting that resources are protected in order, and are
;; released in the opposite order, we know that the operation on the
;; _highest_-ordered resource will never wait or deadlock. By design,
;; the operation holding the lock on the highest resource is not
;; waiting to get a lock on any other resource because such a resource
;; would have to be higher than the highest resource if it is being
;; asked for later.
;;
;; Similarly, two operations won't run concurrently if they both
;; contend for the lowest resource. If one operation takes the lock on
;; the lowest resource, the other operation cannot start--it is
;; waiting to obtain a lock on that lowest resource. If the second
;; operation did start, it would mean that its lowest resource is not
;; the lowest resource of the first operation.
;;
;; Concretely, `exchange(A, B)` and `exchange(B, A)` will not run at
;; the same time becuase both will attempt to lock `A` before locking
;; `B`. Thus, whichever operation gets to `A` first will start running
;; and the other operation has to wait.

(define (serialized-exchange account1 account2)
  (let* ((account1-id (account1 'order-id))
         (account2-id (account2 'order-id))
         (serializer1 (account1 'serializer))
         (serializer2 (account2 'serializer)))
    (if (< (account1 'order-id) (account2 'order-id))
        ((serializer2 (serializer1 exchange)) account1 account2)
        ((serializer1 (serializer2 exchange)) account1 account2))))

;; global account ordering
(define cur-order-id 1)
(define (make-order-id)
  (let ((coi cur-order-id))
    (set! cur-order-id (+ cur-order-id 1))
    coi))

(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer))
        (order-id (make-order-id)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            ((eq? m 'order-id) order-id)
            (else
             (error "Unknown request: MAKE-ACCOUNT"
                    m))))
    dispatch))

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))
