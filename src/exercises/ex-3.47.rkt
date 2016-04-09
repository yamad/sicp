#lang racket
(require "../examples/serializer.rkt")
;; Exercise 3.47 -- A semaphore (of size n) is a generalization of a
;; mutex. Like a mutex, a semaphore supports acquire and release
;; operations, but it is more general in that up to n processes can
;; acquire it concurrently. Additional processes that attempt to
;; acquire the semaphore must wait for release operations. Give
;; implementations of semaphores

;; a. in terms of mutexes

;; first attempt
(require ffi/unsafe/atomic)
(define (make-semaphore-mutex n)
  (let ((locks 0)
        (mutex (make-mutex)))
    (define (acquire-access)
      (cond ((= locks (- n 1))
             (set! locks (+ locks 1))
             (mutex 'acquire)
             'locked)
            ((< locks (- n 1))
             (set! locks (+ locks 1))
             'lock-added)
            (else
             'already-locked)))
    (define (release-access)
      (unless (= locks 0)
        (set! locks (- locks 1)))
      (mutex 'release)
      'unlocked)

    (define (the-semaphore m)
      (call-as-atomic
       (lambda ()
         (cond
          ((eq? m 'acquire) (acquire-access))
          ((eq? m 'release) (release-access))
          ((eq? m 'available) (- n locks))
          (else
           (error "Unknown command -- SEMAPHORE: "
                  m))))))
    the-semaphore))

;; but this seems a little unwieldy. after some googling, it looks
;; like other posted solutions use the embedded mutex to ensure
;; atomicity, and a loop that waits for the count to change as the
;; `lock`
(define (make-semaphore-mutex2 n)
  (let ((mutex (make-mutex))
        (locks 0))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (mutex 'acquire)
             (if (= locks n)
                 ; retry until a lock is released
                 (begin (mutex 'release)
                        (the-semaphore 'acquire))
                 (begin (set! locks (+ locks 1))
                        (mutex 'release))))
            ((eq? m 'release)
             (mutex 'acquire)
             (when (> locks 0)
               (set! locks (- locks 1)))
             (mutex 'release))))
    the-semaphore))

;; b. in terms of atomic `test-and-set!' operations.

;; first attempt
(define (make-semaphore-test n)
  (let ((cell (mcons false 0)))
    (define (semaphore-test-and-set!)
      (call-as-atomic
       (lambda ()
         (cond ((mcar cell) true)
               ((= (mcdr cell) (- n 1))
                (set-mcar! cell true)
                (set-mcdr! cell (+ (mcdr cell) 1))
                false)
               ((< (mcdr cell) (- n 1))
                (set-mcdr! cell (+ (mcdr cell) 1))
                false)))))
    (define (semaphore-release!)
      (call-as-atomic
       (lambda ()
         (unless (= (mcdr cell) 0)
           (set-mcdr! cell (- (mcdr cell) 1)))
         (set-mcar! cell false))))

    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (when (semaphore-test-and-set!)
               (the-semaphore 'acquire))) ; retry
            ((eq? m 'release)
             (semaphore-release!))
            (else
             (error "Unknown message -- SEMAPHORE: " m))))
    the-semaphore))


;; rewrite based on Weiqun Zhang's code
(define (make-semaphore-test2 n)
  (let ((cell (mcons false '()))
        (locks 0))

    (define (acquire-lock)
      (when (test-and-set! cell)
        (acquire-lock)))
    (define (release-lock)
      (clear! cell))

    (define (semaphore-acquire!)
      (acquire-lock)
      (if (= locks n)
          (begin (release-lock)
                 (the-semaphore 'acquire))
          (begin (set! locks (+ locks 1))
                 (release-lock))))
    (define (semaphore-release!)
      (acquire-lock)
      (unless (= locks 0)
        (set! locks (- locks 1)))
      (release-lock))

    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (semaphore-acquire!))
            ((eq? m 'release)
             (semaphore-release!))
            (else
             (error "Unknown message -- SEMAPHORE: " m))))
    the-semaphore))
