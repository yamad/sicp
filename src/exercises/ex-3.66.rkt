#lang racket
(require "../examples/streams.rkt")

;; alternate elements from two streams
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))      ; (s0, t0)
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))              ; (s0, t1), (s0, t2), ...
    (pairs (stream-cdr s) (stream-cdr t))))) ; (s1, t1), ...

;; Exercise 3.66: Examine the stream (pairs integers integers). Can
;; you make any general comments about the order in which the pairs
;; are placed into the stream? For example, approximately how many
;; pairs precede the pair (1, 100)? the pair (99, 100)? the pair (100,
;; 100)? (If you can make precise mathematical statements here, all
;; the better. But feel free to give more qualitative answers if you
;; find yourself getting bogged down.)


;; The stream first emits the first element of each stream as a pair,
;; and then alternates between the "top row" and the rest of the pairs
;; (recursively). That is, as described in the text, the pairs can be
;; decomposed into 3 parts:
;;
;;   i. (s0, t0)       "first element"
;;  ii. (s0, t1...)    "top row"
;; iii. (s1..., t1...) "rest of pairs"
;;
;; (i) is emitted first, and then (ii) and (iii) alternately emit
;; pairs. The first pair from (iii) is the third element in the root
;; stream. Part (iii) is itself a stream of pairs, and because it is
;; interleaved, its pairs emit at half the rate of the root stream.
;;
;; For instance, (pairs integers integers) translates to (pairs [1..]
;; [1..]). Let's call this root, stream A. It's elements are
;;
;; (1 1)                  [first element]
;; (1 2) (1 3) (1 4)...   [top row]
;; (2 2) (2 3) (3 3)...   [rest of pairs, stream B (pairs [2..] [2..])]
;;
;; and are emittted like...
;;
;;    (1 1) (1 2) (2 2) (1 3) (2 3) (1 4) (3 3)
;; A.   i     ii   iii    ii   iii    ii   iii
;;
;; Note that (iii) in stream A is another stream B, which is also
;; decomposed into parts (i-iii):
;;
;;    (2 2) (2 3) (3 3) (2 4) (3 4) (2 5)
;; B.   i     ii   iii    ii   iii    ii
;;
;; and, of course, (iii) of stream B is another stream C:
;;
;;    (3 3) (3 4) (4 4) (3 5) (4 5) (3 6) (5 5)
;; C.   i     ii   iii    ii   iii    ii   iii
;; D.               i           ii         iii
;;
;;
;; The full stream is merged from these elements:
;;
;;    (1 1) (1 2) (2 2) (1 3) (2 3) (1 4) (3 3) (1 5) (2 4) (1 6) (3 4) (1 7)
;; A.   i     ii   iii    ii   iii    ii   iii    ii   iii    ii   iii    ii
;; B.               i           ii         iii          ii         iii
;; C.
;;
;;    (2 5) (1 8) (4 4) (1 9) (2 6)
;; A.  iii    ii   iii    ii   iii
;; B.   ii         iii          ii
;; C.               i

(provide pairs
         interleave)
