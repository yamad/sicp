> Exercise 3.49: Give a scenario where the deadlock-avoidance
> mechanism described above does not work. (Hint: In the exchange
> problem, each process knows in advance which accounts it will
> need to get access to. Consider a situation where a process must
> get access to some shared resources before it can know which
> additional shared resources it will require.)

As the book states, any process that doesn't know from the beginning
which locks it will need to complete obviously cannot guarantee that
it will lock in order (a lower-order resource may become part of the
computation later) and therefore may deadlock.

A contrived example might be a transaction including filtered accounts
that meet some relatively complicated criterion that must be
calculated on the basis of other changes. For instance, say a batch
exchange occurs in which an initial set of accounts are all set to the
sum of the account balances in that set. Then we transfer all balances
from all other accounts in the system with less than this sum. The
fact that the procedure does not know at the beginning which accounts
it will pick in the second round means that two transactions of this
type can deadlock.
