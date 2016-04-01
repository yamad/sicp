> Exercise 3.32 -- The procedures to be run during each time segment
> of the agenda are kept in a queue. Thus, the procedures for each
> segment are called in the order in which they were added to the
> agenda (first in, first out). Explain why this order must be
> used. In particular, trace the behavior of an and-gate whose inputs
> change from 0, 1 to 1, 0 in the same segment and say how the
> behavior would differ if we stored a segment’s procedures in an
> ordinary list, adding and removing procedures only at the front
> (last in, first out)

As the question prompt says, the queue implementation assures a
first-in, first-out order of evaluation for actions in the same time
segment. Let's run through the evalauation of an `and-gate` in FIFO
order.

First, we setup input wires `a` and `b`, and output wire `c`,

```{scheme}
(define a (make-wire))
(define b (make-wire))
(set-signal! a 0)
(set-signal! b 1)

(define c (make-wire))
```

and then connect them with an `and-gate`

```{scheme}
(and-gate a b c)
```

The connection puts an action procedure that sets `c` in the callback
list for both `a` and `b`, which looks like this

```{scheme}
(define (and-action-procedure)
    (let ((new-value
        (logical-and (get-signal a) (get-signal b))))
            (after-delay and-gate-delay
                (lambda ()
                    (set-signal! c new-value)))))
```

The most important line is `(set-signal! c new-value)`, which is the
action that is placed on the agenda. But where does `new-value` come
from? When is it evaluated? It is evaluated when the `set-signal!`
call is _put on the agenda_, not when the `set-signal!` call _is run_.

This means that the `set-signal!` commands must be run in order
because it is only the last command that has the the most current
value of all other signals.

To continue the and-gate example, if we change `a` and `b` simultaneously

```{scheme}
(set-signal! a 1)
(set-signal! b 0)
```

each command places a command to change `c` on the agenda.

```{scheme}
(set-signal! c (logical-and 1 1))  ;; a=1, b=1, b has not changed yet
(set-signal! c (logical-and 1 0))  ;; a=1, b=0, changes to a and b reflected
```

The first command arises from setting `a`, when `b` has not yet been
set, and the second arises from setting `b`, when the change in `a` is
already reflected. These commands obviously must be run in order of
appearance for the correct answer, `c=0`, to hold. If the commands
were run in the opposite last-in first-out order, then we would have
`c=1`. Again, this occurs because when `set-signal! a` gets evaluated
`b` has not changed yet.

Note that direct `set-signal` immediately updates the wire signal. It
is items that get placed on the agenda that may be delayed.
