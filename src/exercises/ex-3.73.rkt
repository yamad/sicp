#lang racket
(require "../examples/streams.rkt")

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

;; Exercise 3.73: We can model electrical circuits using streams to
;; represent the values of currents or voltages at a sequence of
;; times. For instance, suppose we have an RC circuit consisting of a
;; resistor of resistance R and a capacitor of capacitance C in
;; series. The voltage response v of the circuit to an injected current i
;; is determined by the formula in Figure 3.33, whose structure is shown
;; by the accompanying signal-flow diagram.
;;
;;   *Figure 3.33:* An RC circuit and the associated signal-flow
;;   diagram.
;;
;;          +                 -
;;         ->----'\/\/\,---| |---
;;          i                 C
;;
;;                      / t
;;                      |  i
;;         v  =  v   +  |      dt + R i
;;                0     |
;;                      / 0
;;
;;                 +--------------+
;;             +-->|   scale: R   |---------------------+   |\_
;;             |   +--------------+                     |   |  \_
;;             |                                        +-->|    \   v
;;          i  |   +--------------+     +------------+      | add >--->
;;         ----+-->|  scale: 1/C  |---->|  integral  |----->|   _/
;;                 +--------------+     +------------+      | _/
;;                                                          |/
;;
;; Write a procedure RC that models this circuit. RC should take as
;; inputs the values of R , C , and dt and should return a procedure that
;; takes as inputs a stream representing the current i and an ini- tial
;; value for the capacitor voltage v 0 and produces as output the stream
;; of voltages v . For example, you should be able to use RC to model an
;; RC circuit with R = 5 ohms, C = 1 farad, and a 0.5-second time step by
;; evaluating (define RC1 (RC 5 1 0.5)). This de- ﬁnes RC1 as a procedure
;; that takes a stream representing the time sequence of currents and an
;; initial capacitor voltage and produces the output stream of voltages.

(define (RC R C dt)
  (define (circuit i v0)
    (cons-stream
     v0
     (add-streams (scale-stream i R)
                  (integral (scale-stream i (/ 1 C)) v0 dt))))
  circuit)
