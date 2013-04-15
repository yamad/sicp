#lang racket

(require "ex-2.69.rkt")

(define lyrics-tree
  (generate-huffman-tree '((A 2) (BOOM 1) (GET 2) (JOB 2)
                           (NA 16) (SHA 3) (YIP 9) (WAH 1))))

(define msg
  '(GET A JOB
        SHA NA NA NA NA NA NA NA NA
        GET A JOB
        SHA NA NA NA NA NA NA NA NA
        WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
        SHA BOOM))

lyrics-tree
msg
(encode msg lyrics-tree)
(decode (encode msg lyrics-tree) lyrics-tree)
