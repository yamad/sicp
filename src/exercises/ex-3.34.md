> Exercise 3.34 -- Louis Reasoner wants to build a squarer, a
> constraint device with two terminals such that the value of
> connector b on the second terminal will always be the square of the
> value a on the first terminal. He proposes the following simple
> device made from a multiplier:
>
>     (define (squarer a b) (multiplier a a b))
>
>  There is a serious flaw in this idea. Explain.

`multiplier` is only able to calculate a value if two of the three
values in the equation `x * y = z` are known. However, in the
`squarer` device, because `x` and `y` are the same value so there are
not three values, as needed. In this situation, the multiplier works
"forwards" but not in "reverse"--if `z` is known, it also needs to
know either `x` or `y` to calculate the unknown. Since `x` and `y` are
the same, this cannot happen.

It's worth pointing out that mathematically, it is also true that
knowing $z$ in the formula $x^2 = z$ does not uniquely determine $x$,
because it may be $x$ or $-x$. However, the issue with `squarer` is
not related to this case, but rather a mismatch between the
engineering implementation and the pure mathematics. Note, for
example, that the same problem exists for an adder `(adder a a b)` or
`(averager a a b)`, both of which cannot calculate `a` for the same
reason that `multiplier` can't.
