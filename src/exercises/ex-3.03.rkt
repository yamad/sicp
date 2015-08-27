#lang racket

;; exercise 3.3 -- password-protected accounts

(define (make-account balance passwd)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (bad-password x)
    "Incorrect password")
  (define (dispatch p m)
    (cond ((not (eq? p passwd)) bad-password)
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)


;; test
(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40)    ; 60
((acc 'some-other-password 'deposit) 50) ; Incorrect password
