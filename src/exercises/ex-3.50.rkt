#lang racket
;; *Exercise 3.50:* Complete the following definition, which
;; generalizes `stream-map' to allow procedures that take multiple
;; arguments, analogous to `map' in section *Note 2-2-3::, footnote
;; *Note Footnote 12::.
(require "../examples/streams.rkt")

(define (stream-map-multiple proc . argstreams)
  (if (or (stream-null? (car argstreams))
          (ormap stream-null?
                  (map stream-car argstreams)))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map-multiple
              (cons proc (map stream-cdr argstreams))))))

(provide stream-map-multiple)
