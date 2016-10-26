> Exercise 3.57: How many additions are performed when we compute the nth Fibonacci number using the definition of fibs based on the add-streams procedure? Show that the number of additions would be exponentially greater if we had implemented (delay <exp>) simply as (lambda () <exp>), without using the optimization provided by the memo-proc procedure described in Section 3.5.1.

For reference, the `fibs` definition is

```{racket}
(define fibs
   (cons-stream
    0
    (cons-stream 1 (add-streams-count (stream-cdr fibs) fibs))))

(define (add-streams-count s1 s2) (stream-map add+count s1 s2))

(define nadds 0)
(define (add+count . addends)
    (set! nadds (+ 1 nadds))
    (apply + addends))

(define fibs-no-memo
  (cons-stream-no-memo 0
    (cons-stream-no-memo 1
      (add-streams-no-memo (stream-cdr fibs-no-memo) fibs-no-memo))))

(define (stream-map-no-memo proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream-no-memo
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (add-streams-no-memo s1 s2) (stream-map-no-memo add+count s1 s2))
```

`fibs` with memoization takes $n-2$ additions to calculate the $n$th Fibonacci number $\text{Fib}(n)$ (counting from 1--that is, $\text{Fib}(1) = 0, \text{Fib}(2) = 1$); it is order of growth $O(n)$.[^ Some start numbering from 0, so that $\text{Fib}(0) = 0$. In this case, the number of additions in $n-1$].

In this case, all previous numbers are calculated and stored. Computing the next number, $\text{Fib}(n)$, from the previous two numbers takes just one addition. This leads to the initial answer that the $n$th number needs $n$ additions. But because they are given (and not calculated), we don't use any additions for the first two numbers, thus the number of additions is actually $n-2$.

If there is no memoization, however, then a lot of repeated work happens. To calculate $\text{Fib}(n)$, we also recalculate $\text{Fib}(n-1)$ and $\text{Fib}(n-2)$. To do that, we have to walk the entire sequence up to $n$ _twice_ (once for $n-1$ and once for $n-2$) for every step. Symbolically, we can write that the number of additions for the $n$th Fibonacci, $T(n)$ is $T(n) = T(n-1) + T(n-2) + 1$. Notably, this is itself a Fibonacci sequence $T(n) = Fib(T(n)) + 1 > Fib(T(n))$, so order of growth is $O(Fib(n))$

We can also simplify $T(n)$ by noting that $T(n-1) > T(n-2)$ so $T(n) \geq 2T(n-2)$. This means we are doing two things in each reduction step: 1) we multiply by 2 for each step, and 2) we subtract 2 from $n$ in each step. How many times can be subtract 2 from $n$? $n/2$ times. That is, the recursion takes $n/2$ steps of multiplying by 2, or $O(2^(n/2))$. (see Erik Demaine's MIT OCW lecture for the best explanation of this I've come across https://youtu.be/OQ5jsbhAv_M)
