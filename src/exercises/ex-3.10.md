> Exercise 3.10
>
> ```scheme
> (define (make-withdraw initial-amount)
>   (let ((balance initial-amount))
>     (lambda (amount)
>       (if (>= balance amount)
>           (begin (set! balance (- balance amount))
>                  balance)
>           "Insufficient funds"))))
> ```

    ```
      global env
          |
          v
    +----------------------------------+
    |                                  |
    | make-withdraw:                   |
    |  |                               |
    +--|-------------------------------+
       |       ^
       |       |
      (@)-(@)--.
       |
       v
      parameters: initial-amount
      body: (let ...)
      ```

(define W1 (make-withdraw 100))

    ```
      global env
          |
          v
    +----------------------------------+
    |                                  |
    | make-withdraw:                   |
    |  |                               |
    |  |         W1:.                  |
    |  |            |                  |
    +--|------------|------------------+
       |      ^     |       ^
       |      |     |       |
       |      |     |  +---------------------+
       |      |     |  | initial-amount: 100 | E1
       |      |     |  +---------------------+
       |      |     |       ^
       |      |     |       |
       |      |     |  +--------------+
       |      |     |  | balance: 100 | E2
       |      |     |  +--------------+
       |      |     |       ^
       |      |     |       |
       |      |    (@)-(@)--.
       |      |     |
       |      |     v
       |      |    parameters: amount
       |      |    body: (if ...)
       |      |
      (@)-(@)-.
       |
       v
      parameters: initial-amount
      body: (let ...)
      ```

(W1 50)

    ```
      global env
          |
          v
    +----------------------------------+
    |                                  |
    | make-withdraw:                   |
    |  |                               |
    |  |         W1:.                  |
    |  |            |                  |
    +--|------------|------------------+
       |      ^     |       ^
       |      |     |       |
       |      |     |  +---------------------+
       |      |     |  | initial-amount: 100 | E1
       |      |     |  +---------------------+
       |      |     |       ^
       |      |     |       |
       |      |     |  +--------------+
       |      |     |  | balance: 100 | E2
       |      |     |  +--------------+
       |      |     |       ^      ^
       |      |     |       |      |     +-------------+
       |      |    (@)-(@)--.      .-----| balance: 50 | E3*
       |      |     |                    +-------------+
       |      |     v
       |      |    parameters: amount
       |      |    body: (if ...)
       |      |
      (@)-(@)-.
       |
       v
      parameters: initial-amount
      body: (let ...)
      ```

(define W2 (make-withdraw 100))

    ```
      global env
          |
          v
    +-------------------------------------------------------------------------------+
    |                                                                               |
    | make-withdraw:                                                                |
    |  |                                                                            |
    |  |         W1:.                                                               |
    |  |            |                              W2:.                             |
    |  |            |                                                               |
    +--|------------|--------------------------------|------------------------------+
       |      ^     |       ^                        |       ^
       |      |     |       |                        |       |
       |      |     |       |                        |       |
       |      |     |       |                        |       |
       |      |     |       |                        |       |
       |      |     |  +---------------------+       |  +---------------------+
       |      |     |  | initial-amount: 100 | E1    |  | initial-amount: 100 | E3
       |      |     |  +---------------------+       |  +---------------------+
       |      |     |       ^                        |       ^
       |      |     |       |                        |       |
       |      |     |  +-------------+               |  +--------------+
       |      |     |  | balance: 50 | E2            |  | balance: 100 | E4
       |      |     |  +-------------+               |  +--------------+
       |      |     |       ^                        |       ^
       |      |     |       |                        |       |
       |      |    (@)-(@)--.                       (@)-(@)--.
       |      |     |                                |
       |      |     v                                v
       |      |    parameters: amount               parameters: amount
       |      |    body: (if ...)                   body: (if ...)
       |      |
      (@)-(@)-.
       |
       v
      parameters: initial-amount
      body: (let ...)
    ```
