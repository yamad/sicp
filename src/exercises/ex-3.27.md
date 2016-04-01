# Exercise 3.27

## Question

"Memoization" (also called "tabulation") is a technique that enables a
procedure to record, in a local table, values that have previously
been computed.  This technique can make a vast difference in the
performance of a program.  A memoized procedure maintains a table in
which values of previous calls are stored using as keys the arguments
that produced the values.  When the memoized procedure is asked to
compute a value, it first checks the table to see if the value is
already there and, if so, just returns that value.  Otherwise, it
computes the new value in the ordinary way and stores this in the
table.  As an example of memoization, recall from section *Note 1-2-2,
the exponential process for computing Fibonacci numbers:

    (define (fib n)
      (cond ((= n 0) 0)
            ((= n 1) 1)
            (else (+ (fib (- n 1))
                     (fib (- n 2))))))

The memoized version of the same procedure is

    (define memo-fib
      (memoize (lambda (n)
                 (cond ((= n 0) 0)
                       ((= n 1) 1)
                       (else (+ (memo-fib (- n 1))
                                (memo-fib (- n 2))))))))

where the memoizer is defined as

    (define (memoize f)
      (let ((table (make-table)))
        (lambda (x)
           (let ((previously-computed-result (lookup x table)))
             (or previously-computed-result
                 (let ((result (f x)))
                   (insert! x result table)
                   result))))))

Draw an environment diagram to analyze the computation of `(memo-fib
3)`. Explain why `memo-fib` computes the nth Fibonacci number in a
number of steps proportional to n.  Would the scheme still work if we
had simply defined `memo-fib` to be `(memoize fib)`?

## Solution

The starting environment structure looks like this (`lookup` and
`make-table` are in global environment but not pictured):

     ```
     global env
         |
         v
     +----------------+
     |                |
     |      memoize: ---->(@)-(@)
     |                |   parameters: f
     |                |   body: (let ((table ...)) ...)
     |                |
     |                |
     |                |      [this procedure is what gets
     |                |      returned from memoize]
     |                |
     |     memo-fib: ------->(@)-(@) parameters: x
     |                |           |  body: (let ((previously ...
     |                |           |
     |                |   E1      v
     |                |  +-----------+
     |                |<-|       f: ---->(@)-(@)
     |                |  |           |   parameters: n
     |                |  |           |   body: (cond ...
     |                |  |   table: ---->[]
     |                |  +-----------+
     +----------------+
     ```

Note that calls to `memo-fib` operate in environment E1, where the
local table is defined.  This acheived because `memo-fib` is a
_variable definition_, which points to the procedure that is returned
from the call to `memoize`.

Calling `(memo-fib 3)` then evolves environments pointing back to E1
(again, `lookup` and calls to it are not shown):

     ```
     global env
         |
         v
     +----------------+
     |                |
     |      memoize: ---->(@)-(@)
     |                |   parameters: f
     |                |   body: (let ((table ...)) ...)
     |                |
     |     memo-fib: ------->(@)-(@) parameters: x
     |                |           |  body: (let ((previously ...
     +----------------+           |
              ^                   |
       E1     |                   V
     +--------------------------------------+
     |                                      |
     |   f: --------------------------------------->(@)-(@)
     |                                      |       parameters: n
     |                                      |       body: (cond ...
     |                                      |
     |                                      |      +---------------+
     |   table: ---------------------------------->| 1: 1 (via E5) |
     |                                      |      | 0: 0 (via E9) |
     |                                      |      | 2: 1 (via E4) |
     |                                      |      | 3: 2 (via E3) |
     |                                      |      +---------------+
     +--------------------------------------+
         ^
         |   {calls to memo-fib}    [global]  {calls to f}
         |  +------+                   |    +------+
         .--| x: 3 | E2                .----| n: 3 | E3 (via E2)
         |  +------+                   |    +------+
         |  +------++------+           |    +------++------+
         .--| x: 2 || x: 1 | E4/E5     .----| n: 2 || n: 1 | E6/E7 (via E4/E5))
         |  +------++------+           |    +------++------+
         |  +------++------+           |            +------+
         .--| x: 1 || x: 0 | E8/E9     .------------| n: 0 | E10 (via E9)
            +------++------+                        +------+
                                             {note missing second call to f}
                                             {due to memoization}
    ```

Savings are small in this example. Only `(memo-fib 1)` is called
twice, and only one call to `f` is eliminated. But the important point
is that the expression passed to `memoize` is never called again on
identical arguments--that is, no result is calculated twice.

Whereas each call to `fib` (above 0 or 1) results in _two_ more calls
to `fib`, each call to `memo-fib` reduces to just one more call to
`memo-fib`. Thus, an $O(n^2)$ reduces to a $O(n)$ process.

`(memoize fib)` does not work because the procedure must recursively
call `memo-fib`, not `fib`.
