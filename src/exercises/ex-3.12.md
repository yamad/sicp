> Exercise 3.12
>
> Difference between append as a constructor and as a mutator

```scheme
(define x (list 'a 'b))
(define y (list 'c 'd))
```

After these definitions the structures look like:

```
         +---+---+    +---+---+
  x ---> | . |  ----->| . | / |
         +-|-+---+    +-|-+---+
           v            v
          'a           'b

         +---+---+    +---+---+
  y ---> | . |  ----->| . | / |
         +-|-+---+    +-|-+---+
           v            v
          'c           'd
```

(define z (append x y))
(cdr x) ---> (b)

That is, using the constructor version of append gives a separate new
list `z`. Lists `x` and `y` are not altered, so the cdr of x is still
just the pair (b,/).

```
         +---+---+    +---+---+
  x ---> | . |  ----->| . | / |
         +-|-+---+    +-|-+---+
           v            v
          'a           'b

         +---+---+    +---+---+
  y ---> | . |  ----->| . | / |
         +-|-+---+    +-|-+---+
           v            v
          'c           'd

         +---+---+    +---+---+    +---+---+    +---+---+
  z ---> | . |  ----->| . |  ----->| . |  ----->| . | / |
         +-|-+---+    +-|-+---+    +-|-+---+    +-|-+---+
           v            v            v            v
          'a           'b           'c           'd


```

(define w (append! x y))
(cdr x) ---> (b c d)

Using the mutator version of append (append!) results in a change to
the cdr of x itself and x and w are both pointers to the same list.

```
```

  w ---.
       |   +---+---+    +---+---+
  x -----> | . |  ----->| . |  -----.
           +-|-+---+    +-|-+---+   |
             v            v         |
            'a           'b         |
                                    |
                                    |    +---+---+    +---+---+
                                y -----> | . |  ----->| . | / |
                                         +-|-+---+    +-|-+---+
                                           v            v
                                          'c           'd

```