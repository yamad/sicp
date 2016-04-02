> Exercise 3.36 -- Suppose we evaluate the following sequence of
> expressions in the global environment:
>
> (define a (make-connector))
> (define b (make-connector))
> (set-value! a 10 'user)
>
> At some time during evaluation of the `set-value!', the following
> expression from the connector's local procedure is evaluated:
>
> (for-each-except setter inform-about-value constraints)
>
> Draw an environment diagram showing the environment in which the
> above expression is evaluated.

The `for-each-except` call occurs inside of E3 shown below, within the
`a` connector environment E1.

```
  global env
+---------------------------+
|                           |
|  [lots of other defines]  |
|                           |
|  a: --+                   |
|       |                   |
|  b: --|----------------------------+
|       |                   |        |
+-------|-------------------+        |
        |                            |
        |                            |
    E1  v                        E2  v
  +-------------------------+  +-------------------------+
  |           value: 10     |  |           value: #f     |
  |       informant: 'user  |  |       informant: #f     |
  |     constraints: '()    |  |     constraints: '()    |
  |    set-my-value: @-@    |  |    set-my-value: @-@    |
  | forget-my-value: @-@    |  | forget-my-value: @-@    |
  |         connect: @-@    |  |         connect: @-@    |
  |              me: @-@    |  |              me: @-@    |
  +-------------------------+  +-------------------------+
           ^
           |
           |  (set-my-value 10 'user)
    E3     |
  +---------------+
  | newval: 10    |
  | setter: 'user |
  +---------------+

  [in E3]
  (for-each-except 'user inform-about-value '())
```
