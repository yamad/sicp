#lang racket

;; exercise 3.7 -- allow for joint accounts
(define (make-joint acct oldpass newpass)
  (let ((new ((acct oldpass 'add-password) newpass)))
    (if (not new)
        (begin (display "Joint account requested rejected") (newline))
        new)))

;; from exercise 3.3, with additions to handle many passwords
(define (make-account balance passwd)
  (let ((pass-list (list passwd)))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (display-balance) balance)
    (define (add-password p)
      (set! pass-list (cons p pass-list))
      dispatch)
    (define (bad-password x)
      (display "Incorrect password")(newline)
      #f)
    (define (incorrect-password? x)
      (not (ormap (lambda (p) (eq? x p)) pass-list)))
    (define (dispatch p m)
      (cond ((incorrect-password? p) bad-password)
            ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) (display-balance))
            ((eq? m 'add-password) add-password)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))


;; test
(define acc-a (make-account 100 'pass-a))
(define acc-a2 (make-joint acc-a 'pass-a 'pass-b))
(define acc-bad (make-joint acc-a 'pass-wrong 'pass-b))
((acc-a 'pass-a 'withdraw) 40)           ; 60
((acc-a2 'pass-b 'withdraw) 10)          ; 50
((acc-a2 'pass-a 'deposit) 50)           ; 100
(acc-a 'pass-a 'balance)                 ; 100
(acc-a2 'pass-a 'balance)                ; 100
((acc-a 'pass-wrong 'deposit) 50)        ; Incorrect password
