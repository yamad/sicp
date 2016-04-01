#lang racket
;; Exercise 3.29 -- Another way to construct an or-gate is as a
;; compound digital logic device, built from and-gates and inverters.
;; Define a procedure `or-gate' that accomplishes this.  What is the
;; delay time of the or-gate in terms of `and-gate-delay' and
;; `inverter-delay'?

;; or-gate, using inverters and and-gates, based on deMorgan's law
;;
;; not (P and Q) = (not P) or (not Q)    original deMorgan's law
;; P or Q = not ((not P) and (not Q))    invert inputs to AND to recover P OR Q
(define (or-gate-compose a b output)
  (let ((na (make-wire))
        (nb (make-wire))
        (no (make-wire))
    (inverter a na)
    (inverter b nb)
    (and-gate na nb no)
    (inverter no output)
    'ok)))

;; the composed OR gate uses 3 inverters and 1 AND gate, thus the
;; or-gate-compose-delay is (3 inverter-delay + 1 and-gate-delay)
