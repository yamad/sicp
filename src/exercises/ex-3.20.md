> Exercise 3.20
>
> ```scheme
> (define (cons x y)
>   (define (set-x! v) (set! x v))
>   (define (set-y! v) (set! y v))
>   (define (dispatch m)
>     (cond ((eq? m 'car) x)
>           ((eq? m 'cdr) y)
>           ((eq? m 'set-car!) set-x!)
>           ((eq? m 'set-cdr!) set-y!)
>           (else (error "Undefined operation -- CONS" m))))
>   dispatch)
> (define (car z) (z 'car))
> (define (cdr z) (z 'cdr))
> (define (set-car! z new-value)
>   ((z 'set-car!) new-value)
>   z)
> (define (set-cdr! z new-value)
>   ((z 'set-cdr!) new-value)
>   z)
> ```
>
> Draw environment diagrams to illustrate the evaluation of the
> sequence of expressions
>
> ```scheme
> (define x (cons 1 2))
> (define z (cons x x))
> (set-car! (cdr z) 17)
> (car x)
> ```
>
> using the procedural implementation of pairs given above.

    ```
      global env
          |
          v
    +----------------+
    |                |
    |        cons: ------> (@)-(@) parameters: x y
    |                |             body: (define (set-x! ...)
    |         car: ------> (@)-(@) parameters: z
    |                |             body: (z 'car)
    |         cdr: ------> (@)-(@) parameters: z
    |                |             body: (z 'cdr)
    |    set-car!: ------> (@)-(@) parameters: z new-value
    |                |             body: (((z 'set-car!) new-value) z)
    |    set-cdr!: ------> (@)-(@) parameters: z new-value
    |                |             body: (((z 'set-cdr!) new-value) z)
    |                |
    |           x: --------------------------------.
    |                |                             |
    |                |    E1                       |
    |                |   +--------------------+    |
    |                |   |        x: 1 -> 17* |    |
    |                |<--|        y: 2        |    v
    |                |   | dispatch: -----------> (@)-(@) parameters: m
    |                |   |                    |           body: (cond ...)
    |                |   |   set-x!: -----------> (@)-(@) parameters: v
    |                |   |                    |           body: (set! x v)
    |                |   |   set-y!: -----------> (@)-(@) parameters: v
    |                |   |                    |           body: (set! y v)
    |                |   +--------------------+
    |                |          ^   ^
    |                |          |   |                     E5 (set-cdr! x 17)
    |                |          |   |                     [from global env]
    |                |          |   |                    +----------------+
    |                |          .---|--------------------| m: 'set-cdr    |
    |                |              |                    +----------------+
    |                |              |
    |                |              |                     E6 (set-x! 17)
    |                |              |                    +---------------+
    |                |              .--------------------| v: 17         |
    |                |                                   +---------------+
    |                |
    |           z: --------------------------------.
    |                |                             |
    |                |    E2                       |
    |                |   +--------------------+    |
    |                |   |                    |    |
    |                |<--| x: x [E1 dispatch] |    |
    |                |   | y: x [E1 dispatch] |    |
    |                |   |                    |    v
    |                |   |        dispatch: ----> (@)-(@) parameters: m
    |                |   |                    |           body: (cond ...)
    |                |   |          set-x!: ----> (@)-(@) parameters: v
    |                |   |                    |           body: (set! x v)
    |                |   |          set-y!: ----> (@)-(@) parameters: v
    |                |   |                    |           body: (set! y v)
    |                |   +--------------------+
    |                |                   ^                          E4 (cdr z)
    |                |                   |                          [from global]
    |                |                   |                        +------------+
    |                |                   .------------------------| m: 'cdr    |
    |                |                                            +------------+
    |                |
    |                |    E3 (set-car! (cdr z) 17)
    |                |   +------------------------------------+
    |                |<--|         z: (cdr z) --> E1 dispatch |
    |                |   | new-value: 17                      |
    |                |   +------------------------------------+
    |                |      (cdr z)
    |                |      (z 'cdr)
    |                |      (dispatch 'cdr) [E2]
    |                |      y               [E2]
    |                |      x               [E1]
    |                |      dispatch        [E1]
    |                |      [see E4]
    +----------------+
    ```
