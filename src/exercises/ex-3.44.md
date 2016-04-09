
> Exercise 3.44 -- Consider the problem of transferring an amount from
> one account to another.  Ben Bitdiddle claims that this can be
> accomplished with the following procedure, even if there are
> multiple people concurrently transferring money among multiple
> accounts, using any account mechanism that serializes deposit and
> withdrawal transactions, for example, the version of `make-account'
> in the text above.

```{racket}
(define (transfer from-account to-account amount)
  ((from-account 'withdraw) amount)
  ((to-account 'deposit) amount))
```

> Louis Reasoner claims that there is a problem here, and that we
> need to use a more sophisticated method, such as the one required
> for dealing with the exchange problem.  Is Louis right?  If not,
> what is the essential difference between the transfer problem and
> the exchange problem?  (You should assume that the balance in
> `from-account' is at least `amount'.)

There is a bug in this code. If the withdrawal fails because there are
insufficient funds in `from-account`, we need to cancel (or back out
of) the deposit to the `to-account`. This is a problem with the
transfer whether or not it is run concurrently. However, if many
transfers are issued concurrently, then this failure mode is more
likely because the balance in `from-account` can drop below what was
expected when the transfer was initiated. In this sense, Louis is
right that serializing only `withdraw` and `deposit` is not enough to
ensure a safe `transfer`, because whether `deposit` runs depends on
the results of `withdraw`.

If, as specified, in the problem statement, we can be sure that any
and all sequences of transfer calls will never fail during withdrawal,
then Ben is right and serializing just `withdraw` and `deposit` is
enough. Note that the latter part of the exchange problem is, in fact,
the transfer problem. However, as discussed in exercise 3.43, the
issue with concurrency in an exchange is the first step where the
amount to withdraw/deposit is calculated based on current
balances. Such balances could be invalidated by other concurrently
running exchanges before the `withdraw` or `deposit` steps are run.
