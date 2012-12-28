#lang racket

;; exercise 2.20 -- return list of arguments with the same parity
;; (odd/even) as the first argument
(define (same-parity first . rest)
  (define (iter items result)
    (if (null? items)
        result
        (iter (cdr items)
              (if (equal? (even? first)
                          (even? (car items)))
                  (append result (list (car items)))
                  result))))
  (iter rest (list first)))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
