#!/usr/bin/env mzscheme
#lang scheme

;; Pascal's triangle (recursive)
(define (pascal row col)
  (if (or (= row 1) (= col 1) (= row col))
      1
      (+ (pascal (- row 1) (- col 1))
         (pascal (- row 1) col))))


;; Tests
(require (planet schematics/schemeunit:3))
(require (planet schematics/schemeunit:3/text-ui))

(define tests
  (test-suite
   "Tests"
   (check-eq? (pascal 2 1)
              1 "start-of-line")
   (check-eq? (pascal 3 3)
              1 "end-of-line")
   (check-eq? (pascal 3 2)
              2)
   (check-eq? (pascal 5 3)
              6)
   ))
(run-tests tests)