> *Exercise 3.52:* Consider the sequence of expressions

```{racket}
(define sum 0)

(define (accum x)
    (set! sum (+ x sum))
    sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))
; sum = 1, accum has been evaluated on only the first element

; for reference, seq is:
; 1 3 6 10 15 21 28 36 45 55 66 78 91 105 120 136 153 171 190 210

(define y (stream-filter even? seq))
; sum = 6, stream advances until it finds an element that satisfies `even?`
;          works starting at 1, but memoization means `accum` is not called
;          again on the same element

(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
    seq))
; sum = 10, stream advances to 4th element to match predicate

(stream-ref y 7)
; 136, which is the 7th element matching `even?` in `seq`

(display-stream z)
; 10
; 15
; 45
; 55
; 105
; 120
; 190
; 210
```

> What is the value of `sum' after each of the above expressions is
> evaluated?  What is the printed response to evaluating the
> `stream-ref' and `display-stream' expressions?  Would these
> responses differ if we had implemented `(delay <EXP>)' simply as
> `(lambda () <EXP>)' without using the optimization provided by
> `memo-proc'?  Explain.

See inline comments above. Without memoizing the results, `sum` would
be different for each run through `seq` and therefore `seq` itself
would be different because its result depends on the global variable
`sum`.
