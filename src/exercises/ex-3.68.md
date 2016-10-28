> Exercise 3.68: Louis Reasoner thinks that building a stream of
> pairs from three parts is unnecessarily complicated. Instead of
> separating the pair (S0, T0) from the rest of the pairs in the
> first row, he proposes to work with the whole first row, as
> follows:
>
>     (define (pairsLR s t)
>      (interleave
>       (stream-map (lambda (x) (list (stream-car s) x))
>                   t)
>       (pairsLR (stream-cdr s) (stream-cdr t))))
>
> Does this work?
>
> Consider what happens if we evaluate (pairs integers integers)
> using Louis’s definition of pairs.

No. `interleave` does not have lazy evaluation so all arguments are
evaluated immediately when called. This results in an infinite loop
where `pairs` forces evaluation of itself on infinite streams .

Note the second argument to `interleave` `in LR's `pairs' procedure is
a recursive call to `pairs`, which then calls `interleave`, which then
evaluates `pairs`, which then calls `interleave`.
