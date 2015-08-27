#lang racket

;; exercise 3.3 -- password-protected accounts, with call-the-cops function

(define (make-account balance passwd)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))

  ;; security
  (define nbad 0)
  (define (bad-pass-display x)
    "Incorrect password")
  (define (call-the-cops x)
    (error "CALL THE COPS!"))
  (define (protect-dispatch p m)
    (define (bad-password m)
      (set! nbad (+ 1 nbad))
      (if (> nbad 7)
          call-the-cops
          bad-pass-display))

    (if (eq? p passwd)
        (begin (set! nbad 0)
               (dispatch m))
        (bad-password m)))
  protect-dispatch)


;; test
(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40)    ; 60
((acc 'some-other-password 'deposit) 50) ; Incorrect password
((acc 'secret-password 'withdraw) 40)    ; 60

((acc 'some-other-password 'deposit) 50) ; Incorrect password
((acc 'some-other-password 'deposit) 50) ; Incorrect password
((acc 'some-other-password 'deposit) 50) ; Incorrect password
((acc 'some-other-password 'deposit) 50) ; Incorrect password
((acc 'some-other-password 'deposit) 50) ; Incorrect password
((acc 'some-other-password 'deposit) 50) ; Incorrect password
((acc 'some-other-password 'deposit) 50) ; Incorrect password
((acc 'some-other-password 'deposit) 50) ; CALL THE COPS!
