#lang racket

;; concurrently run all procedures `procs`
;; implemented in Racket, for Section 3.4.2 in SICP 2e
(define (parallel-execute . procs)
  (for-each thread procs))

(provide parallel-execute)
