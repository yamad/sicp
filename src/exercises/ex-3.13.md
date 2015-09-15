> Exercise 3.13
>
> ```scheme
> (define (make-cycle x)
>   (set-cdr! (last-pair x) x)
>   x)
>
> (define (last-pair x)
>   (if (null? (cdr x))
>      x
>      (last-pair (cdr x))))
>
> (define z (make-cycle (list 'a 'b' 'c)))
> ```


```
          +---+---+    +---+---+    +---+---+
  z ----- | . |  ----->| . |  ----->| . |  -----.
      ^   +-|-+---+    +-|-+---+    +-|-+---+   |
      |     v            v            v         |
      |    'a           'b           'c         |
      |                                         |
      .-----------------------------------------.
```

Trying to compute (last-pair z) will result in an infinite loop
because there is no cdr that evaulates to null in the list `z`.