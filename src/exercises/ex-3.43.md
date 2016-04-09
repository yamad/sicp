> Exercise 3.43 -- Suppose that the balances in three accounts start
> out as $10, $20, and $30, and that multiple processes run,
> exchanging the balances in the accounts. Argue that if the processes
> are run sequentially, after any number of concurrent exchanges, the
> account balances should be $10, $20, and $30 in some order.

By definition, an exchange interchanges two balances without altering
the amounts (e.g. account A = $10, account B = $20, and after an
exchange A = $20, B = $10). In other words, given an ordering of
account balances $(x, y, z)$, and exchange creates a new permutation
(ordering) (e.g. $(y, x, z)$). By induction, a sequence of exchanges
then creates a sequence of permutations of the original ordering.

Additionally, the `withdraw` on the first account can take place
concurrently with the `deposit` on the second account. To see why, we
can examine the original exchange code:

```{racket}
(define (exchange account1 account2)
    (let ((difference (- (account1 ’balance)
                         (account2 ’balance))))
        ((account1 ’withdraw) difference)
        ((account2 ’deposit) difference)))
```

There are seven (7) individual basic operations in this exchange. We
can divide these seven steps into two groups--the first two steps,
used to calculate the difference, and the rest of the steps, which
carry out the exchange.

```
group 1 steps
-------------
[1] access account1 for diff
[2] access account2 for diff

group 2 steps
-------------
[2a] withdraw account1
  [3] access balance for compare
  [4] access balance for difference
  [5] set balance

[2b] deposit account2
  [6] access balance for addition
  [7] set balance
```

The group 1 steps must occur before the group 2 steps, because the
result from group 1 is used in group 2. However, the two steps within
group 1 can occur in any order.

Similarly, the group 2a (`withdraw`) and group 2b (`deposit`) steps
can be interleaved in any order as long as the set step occurs last
within each subgroup. These set steps are the only opportunity for one
process to interfere with another process, but they must occur
last. Thus, this concurrent operation is possible because the two
accounts interact only in group 1 and group 1 occurs strictly before
the set steps in group 2.

> Draw a timing diagram like the one in Figure 3.29 to show how this
> condition can be violated if the exchanges are implemented using the
> first version of the account-exchange program in this section.

While concurrency is possible in small ways within each exchange, two
separate exchange steps between 3 accounts cannot be run
concurrently. Let's examine why. Without loss of generality, let A =
`account1` with a balance of $10, B = `account2` with a balance of
$20, and C = `account3` wiht a balance of $30.

We can try to concurrently run the following operations:

```{racket}
(exchange A B)
(exchange B C)
```

Of the many possible ways this can go wrong, we show one (where each
line is a step in time):

```
  -- A --                -- B --                   -- C --

+----------+           +----------+              +----------+
| Bal: $10 |           | Bal: $20 |              | Bal: $30 |
+----------+           +----------+              +----------+

 a1: 10                  a1: 20                    a2: 30
             diff: -10
                         a2: 20       diff: -10

 (withdraw)[1]                                    (deposit)[2]
 a1: 10                                            a2: 30
 a1: 10                                            s2: 20
 s1: 20                 (withdraw)[2]            +----------+
+----------+             a2: 20                  | Bal: $20 |
| Bal: $20 |             a2: 20                  +----------+
+----------+             s2: 30 (20 - (-10))          |
     |                 +----------+                   |
     |                 | Bal: $30 |                   |
     |                 +----------+                   |
     |                                                |
     |                                                |
     |                                                |
     |                                                |
     |                  (deposit)[1]                  |
     |                   a1: 30                       |
     v                   s1: 20 (30 + (-10))          v
+----------+           +----------+              +----------+
| Bal: $20 |           | Bal: $20 |              | Bal: $20 |
+----------+           +----------+              +----------+
```

That is, all accounts end up with a balance of $20.

> On the other hand, argue that even with this exchange program, the
> sum of the balances in the accounts will be preserved.

We run through an example for clarity. We have accounts, `A`, `B`, and
`C`. Assume one exchange `Ex1` runs on `A` and `B`. During this first
exchange, the balance of `B` changes once. Now we run a second
exchange `Ex2` on `B` and `C`, concurrently with `Ex1`. Note that both
exchanges attempt to change account `B`. What happens to the balances?

To understand the resulting balances, we need to know _when_ the
difference calculation $d_2$ in `Ex2` is performed. It may occur
either before or after `Ex1` changes the balance on `B`.

```
   Ex1 steps                            Ex2 steps
   ---------                            ---------
   d1 = balance_A - balance_B
                                    <-- d2 occurs here, option B
   update balance_B
                                    <-- d2 occurs here, option A

                                        update balance_B
```

If $d_2$ occurs _after_ `Ex1`'s update (option A) in the above
diagram), then the result is the same as if the exchanges run
sequentially. However, if $d_2$ occurs _before_ the `Ex1` update on
`B`, then the balances will not exchange but the sum of balances is
preserved.

Some notation will help. Let $b_{a0}$ represent the initial balance on
account $a$, and $b_{af}$ represents the final balance on account
$a$. The difference for exchange 1 is then, of course, the difference
between the initial balance on `A` and `B`, or $d_1 = b_{A0} -
b_{B0}$. The final balance on `B` in exchange 1 is $b_{Bf} = b_{B0} +
d_1$ (we assume a deposit, but the math works out either way). The
final balance on `A` is $b_{Af} = b_{A0} - d_1$.

To distinguish operations on account `B` in `Ex2`, we'll rename `B` to
`B'`. Thus, for `Ex2` the difference is $d_2 = b_{B'0} - b_{C0}$, and
the final balance on `B'` is $b_{B'f} = b_{B'1} - d_2$, where
$b_{B'1}$ is either $b_{Bf}$ under option A, or $b_{B0}$ under option
B.

With this notation, showing that balances are preserved means showing that

$$
b_{Af} + b_{B'f} + b_{Cf} = b_{A0} + b_{B0} + b_{C0}
$$

The easy case is if $d_2$ happens after `Ex1`. This is just like a
sequential run, so all balances of `B` in `Ex2` are equal to the final
`B` balance from `Ex1` $b_{Bf}$. That is, $b_{B'0} = b_{B'1} =
b_{Bf}$, and $b_{B'f} = b_{Bf} - (b_{Bf} - b_{C0}) = b_{C0}$. The
final balance on `C` is $b_{Cf} = b_{C0} + (b_{Bf} - b_{C0}) =
b_{Bf}$. And because `Ex1` completes before `Ex2`, $b_{Af} = b_{B0}$
and $b_{Bf} = b_{A0}$.

Thus,

$$
b_{Af} + b_{B'f} + b_{Cf} = b_{Af} + b_{C0} + b_{Bf} = b_{B0} + b_{C0} + b_{A0}.
$$

Now we've exercised the notation, and we tackle the more complicated
case. If $d_2$ happens before `Ex1`'s update (option B), then the
initial and intermediate `B` balances in `Ex2` are different. Namely,
$b_{B'0} = b_{B0}$, but $b_{B'1} = b_{Bf}$. Thus, the final balance on
`B` is $b_{B'f} = b_{Bf} - (b_{B0} - b_{C0})$.

We can also calculate $b_{Cf} = b_{B0}$, and $b_{Af} = b_{B0}$. So
both account `A` and `C` end up with the original balance on account
`B`. Does the final balance on `B` equal this out? Well, from `Ex1` we
know that $b_{Bf} = b_{B0} + d_1$, and thus $b_{B'f} = (b_{B0} +
d_1) - (b_{B0} - b_{C0}) = d_1 + b_{C0}$ = (b_{A0} - b_{B0}) +
b_{C0}$. That is, the final balance `B` is the sum of the original
balances on accounts `A` and `C` minus the original balance on account
`B`. Plugging in, we find,

$$
\begin{aligned*}
b_{Af} + b_{B'f} + b_{Cf} &= b_{B0} + (b_{A0} + (-b_{B0}) + b_{C0}) + b_{B0} \\
                          &= b_{B0} + b_{A0} + b_{C0}.
\end{aligned*}
$$

Because the withdraw and deposit steps are atomic and the calculation
they make is determined once the difference calculation is performed,
the order of these steps does not change the result. For cases where
the `Ex2` difference calculation happens before the `Ex1` calculation,
the same argument applies with the names reversed.

> Draw a timing diagram to show how even this condition would be
> violated if we did not serialize the transactions on individual
> accounts.

The above reasoning assumes that the `withdraw` and `deposit` actions
happen atomically on each account. In particular, the sum of the
balances is not preserved if the final set balance operations are
interleaved. Note, for instance, how the deposit on `B` runs right
after all the calculations for the withdrawal on `B` are done but
before the balance is set by the withdrawal.

```
  -- A --                -- B --                   -- C --

+----------+           +----------+              +----------+
| Bal: $20 |           | Bal: $20 |              | Bal: $30 |
+----------+           +----------+              +----------+


 a1: 30                  a1: 20                    a2: 30
             diff: 20
                         a2: 20       diff: -20

 (withdraw)[1]                                    (deposit)[2]
 a1: 30                                            a2: 20
 a1: 30                                            s2: 10
 s1: 10                 (withdraw)[2]            +----------+
+----------+             a2: 20                  | Bal: $10 |
| Bal: $10 |             a2: 20                  +----------+
+----------+                                          |
     |                  (deposit)[1]                  |
     |                   a1: 20                       |
     |                   s1: 10 (20 + (-10))          |
     |                 +----------+                   |
     |                 | Bal: $10 |                   |
     |                 +----------+                   |
     |                                                |
     |                                                |
     |                  (withdraw)[2]                 |
     v                   s2: 30 (20 - (-10))          v
+----------+           +----------+              +----------+
| Bal: $10 |           | Bal: $30 |              | Bal: $10 |
+----------+           +----------+              +----------+
```
