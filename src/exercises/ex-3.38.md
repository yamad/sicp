> Exercise 3.38 -- Suppose that Peter, Paul, and Mary share a joint
> bank account that initially contains $100. Concurrently, Peter
> deposits $10, Paul withdraws $20, and Mary withdraws half the money
> in the account, by executing the following commands:
>
> Peter: (set! balance (+ balance 10))
> Paul: (set! balance (- balance 20))
> Mary: (set! balance (- balance (/ balance 2)))
>
> a. List all the different possible values for balance after these three
> transactions have been completed, assuming that the banking
> system forces the three processes to run sequentially in some
> order.

Pe/Pa/Ma: $45
Pe/Ma/Pa: $35
Pa/Pe/Ma: $45
Pa/Ma/Pe: $50
Ma/Pe/Pa: $35
Ma/Pa/Pe: $35

> b. What are some other values that could be produced if the system
> allows the processes to be interleaved? Draw timing diagrams
> like the one in Figure 3.29 to explain how these values
> can occur.

For example, Mary's operation has two separate accesses to balance. If
it is interleaved with another operation, and the balance is retrieved
two separate times, something like the following could happen:

```
Peter        Balance        Mary
             +-----+
    +--------| 100 |---------+
    |        +-----+         |
    v                        |
 a: 100                      v
                          a: 100
                         nv:  50 (/ 100 2)
nv: 110
 s: 110
    |        +-----+
    +------->| 110 |----> a: 110
             +-----+     nv:  60 (- 110 50)

                          s:  60
                             |
             +-----+         |
             |  60 |<--------+
             +-----+
```
