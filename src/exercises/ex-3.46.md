
> Exercise 3.46 -- Suppose that we implement test-and-set! using an
> ordinary procedure as shown in the text, without attempting to make
> the operation atomic. Draw a timing diagram like the one in Figure
> 3.29 to demonstrate howthe mutex implementation can fail by allowing
> two processes to acquire the mutex at the same time.

We've analyzed similar situations many times by now. The issue is code
that tests the state of a variable and then takes an action based on
the result of that test. But if the state of the variable changes
between when the test and the conditional code is run, then the result
of the test can be invalidated.

Here, the `test-and-set!` code is

```{racket}
(define (test-and-set! cell)
   (if (car cell) true (begin (set-car! cell true) false)))
;;     [1]                    [2]
```

where the value of `cell` is accessed at [1] and it is toggled at [2]
only if the test at [1] is false. Thus, if `test-and-set!` is not
atomic, then two separate calls to `test-and-set!` may both execute
[1] before [2] is executed in either call. When this happens both
calls return `false`, and both callers will believe they have locked
the mutex and run the rest of the code assuming they have exclusive
access.
