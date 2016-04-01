> Exercise 3.31 -- The internal procedure `accept-action-procedure!`
> defined in `make-wire` specifies that when a new action procedure is
> added to a wire, the procedure is immediately run. Explain why this
> initialization is necessary. In particular, trace through the
> `half-adder` example in the paragraphs above and say how the system’s
> response would differ if we had defined `accept-action-procedure!` as

    (define (accept-action-procedure! proc)
        (set! action-procedures
            (cons proc action-procedures)))

There are two things needed for the circuit to work correctly. First,
the relationship between wires needs to be set up. This is achieved by
the first part of `accept-action-procedure!`. Second, all wires must
start in a consistent state (e.g. if `a` and `b` are connected via an
inverter, `a` and `b` should hold opposite signals). Establishing the
initial consistent state is the role of the initial call to the
procedure.

If the wires are not in a consistent state, then signal changes may
fail to propagate. Because each wire only fires actions when its value
_changes_ (see `set-my-signal!`), its possible for an wrongly set wire
to stop transmission of a signal.

For example, in `half-adder`, the internal wire `e` is set to 0 on
construction (because that's the default when there is a new
wire). But, instead, `e` should start as 1 if all the ingoing wires
are 0:

```{scheme}
(define a (make-wire)) ;; is 0
(define b (make-wire)) ;; is 0
(define s (make-wire)) ;; is 0
(define c (make-wire)) ;; is 0
(half-adder a b s c)
```

Inside, `half-adder`, wire `e` inverts wire `c`. So because `c` starts
at 0, `e` should start at 1.

```{scheme}
(set-signal! e (logical-not c))
```

With `e` starting at 1, the half-adder works correctly. If, say, `a`
switches to 1, then `d` switches to 1, and `s` switches
to 1. Importantly, however, `c` does not change and this means that
`e` is not signaled to change either. That is,

```{scheme}
;; d = 0, e = 1 (hidden state)
(set-signal! a 1)

;; inside the half-adder
(set-signal! d (logical-or 1 0))  ;; d = 1, SWITCH
(set-signal! c (logical-and 1 0)) ;; c = 0, NO CHANGE
;; no change in `c` so no action created for `e`

;; d = 1, e = 1
(set-signal! s (logical-and d e)) ;; s = 1, SWITCH
```

However, say we use the version of `accept-action-procedure!` shown
above (without the direct procedure call). In this case, `e` is 0,
because it has only been placed in `c`'s action list but it is not
initialized. It will only change when `c` changes.

How does this go wrong? In this case if we switch `a` (or `b`)
to 1, `s` stays at 0 when it should be 1:

```{scheme}
;; d = 0, e = 0
(set-signal! a 1)

;; inside the half-adder
(set-signal! d (logical-or 1 0))  ;; d = 1, SWITCH
(set-signal! c (logical-and 1 0)) ;; c = 0, NO CHANGE
;; no change in `c` so no action created for `e`

;; d = 1, e = 0
(set-signal! s (logical-and d e)) ;; s = 0, NO CHANGE, WRONG!
```

In fact, in order to get `e` into the right state, we have to force
`c` to change.

```{scheme}
(set-signal! a 1)
(set-signal! b 1)
;; ...
(set-signal! c (logical-and 1 1)) ;; c = 1, SWITCH
(set-signal! e (logical-not c))   ;; e = 0
```

Now `c` and `e` are in the right relation to each other, but `e` still
hasn't propagated a signal yet. On the _next_ change to `c`, the
circuit will finally work properly.
