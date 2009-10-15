.. Structure and Interpretation of Computer Programs Notes
.. :Author: Jason Yamada-Hanff

==================================================
 Chapter 1. Building Abstractions with Procedures
==================================================
:Started: 2008-02-03


1.1 Elements of Programming
===========================

Three main mechanisms for combining ideas:

 primitive expressions
     simple entities

 means of combination
     build compound elements out of simple ones

 means of abstraction
     naming of compound elements

Two kinds of elements in programming:

 * procedures
 * data


1.1.1 Expressions
-----------------

 * `prefix notation`

.. code-block:: scheme

   ;; prefix notation
   > (+ 3 4)
   7
   > (+ (* 2 2) (/ 4 2))
   6
..

 * every expression has a value


1.1.2 Naming and the Environment
--------------------------------

.. code-block:: scheme

   ;; naming with `define`
   > (define size 2)
   > size
   2

environment
    the memory space that keeps track of name-object pairs (variables and
    their values)


1.1.3 Evaluating Combinations
-----------------------------

To evaluate a combination (e.g. ``(+ 3 2)``):

 #. Evaluate the subexpressions of the combination
 #. Apply the procedure (leftmost subexpression) to the arguments (the other subexpressions)

.. note:: This evaluation procedure is *recursive*

tree accumulation
    a general process by which values percolate upwards
special form
    a language construct that does not follow the evaluation rule (e.g. ``define``)


1.1.4 Compound Procedures
-------------------------

Procedures are generally defined by:

.. code-block:: scheme

 (define (<name> <formal parameters>) <body>)

 ;; an example:
 (define (square x) (* x x))


Two different, and separable, operations are performed:

 * creating a procedure
 * naming the procedure

Once defined, compound procedures are indistinguishable from primitive procedures.


1.1.5 Substitution Model
------------------------

**Substitution model** for procedure application
  To apply a compound procedure to arguments, evaluate the body of the
  procedure with each formal parameter replaced by the corresponding
  argument.

An example:

.. code-block:: scheme

 ;; given the definitions
 (define (square x) (* x x))

 (define (sum-of-squares x y)
   (+ (square x) (square y)))

 (define (f a)
   (sum-of-squares (+ a 1) (* a 2)))

 ;; the substitution model models evaluation by expansion, so
 
 (f 5)                               ; this expression
 (sum-of-squares (+ a 1) (* a 2))    ; will expand to its definition
 (sum-of-squares (+ 5 1) (* 5 2))    ; and parameters are replaced by arguments.

 (sum-of-squares 6 10)               ; then, evaluate subexpressions,
 (+ (square x) (square y)            ; retrieve body of `sum-of-squares`,
 (+ (square 6) (square 10)           ; replace parameters,
 (+ (* 6 6) (* 10 10))               ; retrieve body of `square`,
 (+ 36 100)                          ; evaluate subexpressions,
 136                                 ; and evaluate the primitive combination.

The substitution model is a *model* -- simplified and incomplete.  More
complex and refined models will show up later.

Applicative order vs normal order
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

normal-order evaluation
    "fully expand and then reduce" evaluation model.  In this model,
    expressions expand until only primitive expressions are left.  The
    expressions are then evaluated (reduced) to the final answer

applicative-order evaluation
    "evaluate the arguments and then apply" evaluation model.  This is the
    model that Lisp actually uses.

For procedures that can be modeled by substitution, these models produce the
same value.  But they *may* give different results in particular contexts.


1.1.6 Conditional Expressions and Predicates
--------------------------------------------

``cond`` -- Scheme construct for a *case analysis*
    setup as a series of *clauses*, with a *predicate* and a *consequent
    expression*.

Predicates evaluate to *true* (``#t``) or *false* (``#f``).  All values are
true, unless they are false.

.. code-block:: scheme

   (cond (<p1> <e1>)
         (<p2> <e2>)
         (<pn> <en>))

   ;; example
   (cond ((> x 0) x)
         ((= x 0) 0)
         ((< x 0) (- x)))

``if`` -- restricted conditional handling two cases only

.. code-block:: scheme
   
   (if <predicate> <consequent> <alternative>)

   ;; example
   (if (< x 0) (- x) x))

``and``, ``or``, and ``not`` -- logical composition operations

.. code-block:: scheme
   
   (and <e1> ... <en>)
   ;; false if any <e>'s are false. exits evaluation on first false

   (or <e1> ... <en>)
   ;; value of the first true <e>. false if no <e>'s are true

   (not <e>)
   ;; true when <e> is false. false otherwise

Exercises
---------

1.1
~~~

.. literalinclude:: src/exercises/ch1/ex-1.1.scm
   :language: scheme

1.2
~~~
Convert the following into `prefix notation`:

.. math::

   \frac{5+4+(2-(3-(6+\frac{4}{5})))}{3(6-2)(2-7)}

.. literalinclude:: src/exercises/ch1/ex-1.2.scm
   :language: scheme

1.3
~~~

.. literalinclude:: src/exercises/ch1/ex-1.3.scm
   :language: scheme

1.4
~~~

.. literalinclude:: src/exercises/ch1/ex-1.4.scm
   :language: scheme

Operators can be compound expressions.  This procedure will add ``a`` and
``b`` when ``b`` is greater than 0, and subtract ``b`` from ``a`` otherwise.
Operators are subject to expression constructs just like operands, which is
very cool.

1.5
~~~

Applicative-order interpreters will never terminate, because the evaluation of
the operand ``(p)`` creates an infinite loop.

Normal-order interpreters will go step-by-step and never need to hit ``(p)``
since the ``if`` predicate is true.

.. literalinclude:: src/exercises/ch1/ex-1.5.scm
   :language: scheme


1.1.7 Example: Newton's Square Root Approximation
-------------------------------------------------

declarative descriptions/knowledge
    *what is* or describing the properties of things

imperative descriptions/knowledge
    *how to* or describing how to do things

In general, computer science is concerned with imperative knowledge,
where mathematics is more interested in declarative knowledge.

For instance, a mathematical description of the square-root function would be:

.. math::
   \sqrt{x} = \mathrm{the}\ y\ \mathrm{ such\ that}\ y \geq 0\ \mathrm{and}\ y^2 = x

A pseudo-Lisp definition is not useable for calculation either:

.. code-block:: scheme
   
   (define (sqrt x)
     (the y (and (>= y 0)
                 (= (square y) x))))

Newton's method of successive approximations
    Given a guess :math:`y` for the value of :math:`\sqrt{x}`, a
    better guess can be obtained by averaging :math:`y` with
    :math:`x/y`

Simple lisp code for this method would go as follows:

.. literalinclude:: src/examples/ch1/sqrt.scm
   :language: scheme

.. note:: The language constructs learned so far constitutes a
   Turing-complete language (I think this is what they are trying to
   say).  With implementation of tail recursion in Scheme, procedure
   calls like in `sqrt-iter` can substitute for special looping and
   iteration constructs.


*Exercises*
-----------

1.6
~~~

`if` is implemented as special form so that it can use normal-order
evaluation rather than applicative-order evaluation.  While a
user-defined `if` function would have the same syntax as the built-in
special `if`, it would use applicative-order evaluation like `cond`.
Since applicative-order evaluation always evaluates all
arguments/subexpressions, a procedure that calls itself would create
an infinite loop.  Normal-order evaluation only evaluates arguments as
needed, and so would not have this problem.

An example makes this clear:

.. literalinclude:: src/exercises/ch1/ex-1.6.scm
   :language: scheme

Since the `sqrt-iter` procedure calls itself in the else-clause, the
`new-if` version of this procedure will create an infinite loop as the
else-clause will evaluate over and over even when it is not needed.

1.7
~~~

`good-enough?` based on a predeterminate value is inappropriate for
small numbers because the defined tolerance will stop the
approximation too soon.  That is, a predefined tolerance assumes a
certain order of magnitude of the input numbers.  For instance, if the
starting number is already smaller than the tolerance, the difference
between it and a guess will definitely be smaller than the tolerance
and the approximation will stop on the first try.

For large numbers, if calculation precision is poor, the machine will
not be able to represent any change in the numbers at all and will
therefore never terminates.  Thus, in both cases, an absolute
tolerance does not handle edge cases well.

The code below shows the problem with the old version, and an updated
`good-enough?` function that uses a relative tolerance:

.. literalinclude:: src/exercises/ch1/ex-1.7.scm
   :language: scheme


1.8
~~~

An implementation of Newton's approximation method for cube roots,
where an approximation `y` for the cube root of `x` can be improved
by:

.. math::
   \frac{x/y^2 + 2y}{3}

.. literalinclude:: src/exercises/ch1/ex-1.8.scm
   :language: scheme

1.1.8 Procedures as Black-Box Abstractions
------------------------------------------

Decomposition of a problem into subproblems.  Note this was done in
Newton's sqrt approximation example.

 * each procedure should carry out an identifiable task
 * procedures that hide implementation can be used modularly

 procedural abstraction 
     to use a procedure, should be able to disregard implementation (*how*)

Local names
~~~~~~~~~~~

Names used within a procedure should not affect names--potentially the
same--outside of the procedure.

Inside a procedure, formal parameter names do not matter.  The
procedure binds those names as *bound variables*.  The names only have
meaning within the *scope* of the procedure.  The alternative is *free
variables*.  These names matter since they reference definitions
external to the procedure.

 scope
     the set of expressions for which a binding defines a name

Internal definitions and block structure
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 block structure
     procedures that are only relevant as subprocedures of other procedures
     can be hidden within the parent procedure.  The subprocedures are then
     local to the parent procedures.  *Always include all definitions before use*

.. code-block:: scheme

   (define (sqrt-iter guess x)
     (if (good-enough? guess x)
         guess
         (sqrt-iter (improve guess x)
                     x)))

   (define (improve guess x)
     (average guess (/ x guess)))

   (define (average x y)
     (/ (+ x y) 2))

   (define (good-enough? guess x)
     (< (abs (- (square guess) x)) 0.001))

   (define (square x) (* x x))

   (define (sqrt x)
     (sqrt-iter 1.0 x))

Only `sqrt` is important to other users, so use block structure:

.. code-block:: scheme

   (define (square x) (* x x))
   (define (average x y)
     (/ (+ x y) 2))

   (define (sqrt x)
     (define (improve guess x)
       (average guess (/ x guess)))
     (define (good-enough? guess x)
       (< (abs (- (square guess) x)) 0.001))
     (define (sqrt-iter guess x)
       (if (good-enough? guess x)
           guess
           (sqrt-iter (improve guess x) x)))
     (sqrt-iter 1.0 x))

**lexical scoping** -- use variables bound in enclsoing scope as free
variables in subroutines

.. code-block:: scheme

   ;; remove x as formal parameter from subprocedures, already defined by sqrt
   (define (sqrt x)
     (define (improve guess)
       (average guess (/ x guess)))
     (define (good-enough? guess)
       (< (abs (- (square guess) x)) 0.001))
     (define (sqrt-iter guess)
       (if (good-enough? guess)
           guess
           (sqrt-iter (improve guess))))
     (sqrt-iter 1.0))

1.2 Procedures and the Processes They Generate
==============================================

"The ability to visualize the consequences of the actions under
consideration is crucial to becoming an expert programmer."

*local evolution* of a computational process is described by a procedure.
Will be one of a set of common patterns.

1.2.1 Linear Recursion and Iteration
------------------------------------

Linear Recursive Process
~~~~~~~~~~~~~~~~~~~~~~~~

 * evolution is characterized by an expansion then a contraction
 * builds a chain of *deferred operations*
 * number of operations grows linearly

.. code-block:: scheme

   (define (factorial n)
     (if (= n 1)
         1
         (* n (factorial (- n 1))))

   (factorial 4)
   (* 4 (factorial 3))
   (* 4 (* 3 (factorial 2))
   (* 4 (* 3 (* 2 (factorial 1))))
   (* 4 (* 3 (* 2 1)))
   (* 4 (* 3 2))
   (* 4 6)
   24

Linear Iterative Process
~~~~~~~~~~~~~~~~~~~~~~~~

 * a process whose state is characterized by *state variables*
 * a rule describes how the state variables change
 * computation is fully described at any time by state
 * evolution is a constant set of operations
 * number of *steps* grows linearly

.. code-block:: scheme

   (define (factorial n)
     (fact-iter 1 1 n))

   (define (fact-iter product counter max-count)
     (if (> counter max-count)
         product
         (fact-iter (* counter product)
                    (+1 counter)
                    max-count)))

   (factorial 4)
   (fact-iter 1 1 4)
   (fact-iter 1 2 4)
   (fact-iter 2 3 4)
   (fact-iter 6 4 4)
   (fact-iter 24 5 4)
   24

Process vs. Procedure (Recursion)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 recursive procedure
     a procedure definition that refers to/calls itself

 recursive process
     a process that evolves by calling itself and building a chain of deferred operations

An iterative process can be described by a recursive procedure.  This
is true of the iterative process for factorials above.

 tail-recursive
     a property of a language implementation that executes an
     iterative process in constant space, even if the procedure is
     recursive.


Exercises
---------

1.9
~~~

.. literalinclude:: src/exercises/ch1/ex-1.9.scm
   :language: scheme

Procedure 1 is recursive, while procedure 2 is iterative.

1.10
~~~~

Ackerman's function.  A confusing one.  Also, this is a different
Ackerman's function than is defined elsewhere.

Expanding from inside out instead of outside in is the only way to do
this reasonably.  The general rule is still unclear to me, but:

 * function `f` (:math:`A(0,n)`) -- :math:`2n`
 * function `g` (:math:`A(1,n)`) -- :math:`2^n`
 * function `h` (:math:`A(2,n)`) -- :math:`2^{2^{2\ldots n}}`, :math:`2^{h(n-1)}`

.. literalinclude:: src/exercises/ch1/ex-1.10.scm
   :language: scheme


1.2.2 Tree Recursion
--------------------

Fibonacci as an example of a *tree recursive process*

.. code-block:: scheme

   (define (fib n)
     (cond ((= n 0) 0)
           ((= n 1) 1)
           (else (+ (fib (- n 1))
                    (fib (- n 2))))))


This process evolves like a *tree*, because each problem is branched
into two `fib` calls.  Lot of redundancy, `fib(n)` may be recalculated
many times.

 * number of steps proportional to number of nodes in tree
 * space required proportional to max depth of tree


An iterative implementation

.. code-block:: scheme

   (define (fib n)
     (fib-iter 1 0 n))

   (define (fib-iter a b count)
     (if (= count 0)
         b
         (fib-iter (+ a b) a (- count 1))))


This iterative version is much faster.  But recursive definitions are
often more intuitive (closer to the actual description of the
process).


Example: Counting Change
~~~~~~~~~~~~~~~~~~~~~~~~

"How many ways to change $1.00 given half-dollars, quarters, dimes, nickels, and pennies?"

Ways to make change divided into two groups: ones that do not use the
"first kind" of coin, and those that do.

.. literalinclude:: src/examples/ch1/count-change.scm
   :language: scheme

TODO: An interative version of `count-change`


Exercises
---------

1.11
~~~~

A recursive and iterative process for the function :math:`f`, defined
by:

.. math::

   f(n) = \left\{
   \begin{array}{ll}
       n, & \mbox{if $n < 3$} \\
       f(n-1) + 2f(n-2) + 3f(n-3), & \mbox{if $n \geq 3$} \\
   \end{array}
   \right.

.. literalinclude:: src/exercises/ch1/ex-1.11.scm
   :language: scheme

1.12
~~~~

Pascal's triangle

.. literalinclude:: src/exercises/ch1/ex-1.12.scm
   :language: scheme

1.13
~~~~

TODO: Mathematical proof of properties of Fibonacci numbers

1.2.3 Orders of Growth
----------------------

 orders of growth
     gross measure of resources required by a process as inputs are larger

If :math:`n` is a measure of problem size, :math:`R(n)` is the amount
of resources required for a problem of size :math:`n`.

 * :math:`n` can be any measure that is appropriate

:math:`R(n) = \theta (f(n))` (has order of growth, not actually an equivalency)

 * :math:`\theta (n)` -- linear
 * :math:`\theta (1)` -- constant
 * :math:`\theta (n^2)` -- exponential

Big-O is a crude description.  Generally tracks the largest power polynomial (i.e. :math:`1000n^2` and :math:`3n^2 + 2n + 1` and :math:`n^2` are all :math:`\theta (n^2)`).

.. note::
   
   A very useful shorthand that I don't totally understand the
   underlying math of, found at
   <http://www.neowin.net/forum/lofiversion/index.php/t178833.html>.

    * If adding/subtracting the constant `m` to the index (controlling
      variable), `n`, then growth will be :math:`\theta
      (\dfrac{n}{m})`.
    * If multiplying/dividing by the constant `m` from the index `n`,
      then growth will be :math:`\theta (\log_m n)`.

   Not sure if this holds completely.

1.14
~~~~

The `count-change` function above has space requirements that grow
linearly (:math:`\theta (n)`), since tree depth basically depends on
counting down to zero from the initial amount (the calculation for
pennies).  Number of steps is exponential growth (:math:`\theta
(n^2)`) because each step generates two more steps.

(Apparently this is wrong and number of steps is much bigger,
:math:`\theta (n^c)`, where :math:`c` is the number of coins.)

.. digraph:: cc

   node[shape=plaintext];
   cc11 -> cc11_5;
   cc11_5 -> cc11_4;
   cc11_5 -> cc_39_5;
   cc11 [label="(count-change 11)"];
   cc11_5 [label="(cc 11 5)"];
   cc11_4 [label="(cc 11 4)"];
   cc_39_5 [label="(cc -39 5)", fontcolor=red];

   cc11_4 -> cc11_3;
   cc11_4 -> cc_14_4;
   cc11_3 [label="(cc 11 3)"];
   cc_14_4 [label="(cc -14 4)", fontcolor=red];

   cc11_3 -> cc11_2;
   cc11_3 -> cc1_3;
   cc11_2 [label="(cc 11 2)"];
   cc1_3 [label="(cc 1 3)"];

   cc1_3 -> cc1_2;
   cc1_3 -> cc_9_3;
   cc_9_3 [label="(cc -9 3)", fontcolor=red];
   cc1_2 [label="(cc 1 2)"];

   cc1_2 -> cc1_1_a;
   cc1_2 -> cc_4_2;
   cc1_1_a [label="(cc 1 1)"];
   cc_4_2 [label="(cc -4 2)", fontcolor=red];

   cc1_1_a -> cc1_0_a;
   cc1_1_a -> cc0_1_a;
   cc1_0_a [label="(cc 1 0)", fontcolor=red];
   cc0_1_a [label="(cc 0 1)", fontcolor=green];

   cc11_2 -> cc11_1;
   cc11_2 -> cc6_2;
   cc11_1 [label="(cc 11 1)"];
   cc6_2 [label="(cc 6 2)"];

   cc6_2 -> cc6_1;
   cc6_2 -> cc1_2_b;
   cc6_1 [label="(cc 6 1)"];
   cc1_2_b [label="(cc 1 2)"];

   cc1_2_b -> cc1_1_b;
   cc1_2_b -> cc_4_2_b;
   cc1_1_b [label="(cc 1 1)"];
   cc_4_2_b [label="(cc -4 2)", fontcolor=red];

   cc1_1_b -> cc1_0_b;
   cc1_1_b -> cc0_1_b;
   cc1_0_b [label="(cc 1 0)", fontcolor=red];
   cc0_1_b [label="(cc 0 1)", fontcolor=green];

   cc6_1 -> cc6_0;
   cc6_1 -> cc5_1;
   cc6_0 [label="(cc 6 0)", fontcolor=red];
   cc5_1 [label="(cc 5 1)"];

   cc5_1 -> cc5_0;
   cc5_1 -> cc4_1;
   cc5_0 [label="(cc 5 0)", fontcolor=red];
   cc4_1 [label="(cc 4 1)"];

   cc4_1 -> cc4_0;
   cc4_1 -> cc3_1;
   cc4_0 [label="(cc 4 0)", fontcolor=red];
   cc3_1 [label="(cc 3 1)"];

   cc3_1 -> cc3_0;
   cc3_1 -> cc2_1;
   cc3_0 [label="(cc 3 0)", fontcolor=red];
   cc2_1 [label="(cc 2 1)"];
   
   cc2_1 -> cc2_0;
   cc2_1 -> cc1_1_c;
   cc2_0 [label="(cc 2 0)", fontcolor=red];
   cc1_1_c [label="(cc 1 1)"];   

   cc1_1_c -> cc1_0_c;
   cc1_1_c -> cc0_1_c;
   cc1_0_c [label="(cc 1 0)", fontcolor=red];
   cc0_1_c [label="(cc 0 1)", fontcolor=green];

   cc11_1 -> cc11_0;
   cc11_1 -> cc10_1;
   cc11_0 [label="(cc 11 0)", fontcolor=red];
   cc10_1 [label="(cc 10 1)"];

   cc10_1 -> cc10_0;
   cc10_1 -> cc9_1;
   cc10_0 [label="(cc 10 0)", fontcolor=red];
   cc9_1 [label="(cc 9 1)"];

   cc9_1 -> cc9_0;
   cc9_1 -> cc8_1;
   cc9_0 [label="(cc 9 0)", fontcolor=red];
   cc8_1 [label="(cc 8 1)"];

   cc8_1 -> cc8_0;
   cc8_1 -> cc7_1;
   cc8_0 [label="(cc 8 0)", fontcolor=red];
   cc7_1 [label="(cc 7 1)"];

   cc7_1 -> cc7_0;
   cc7_1 -> cc6_1_b;
   cc7_0 [label="(cc 7 0)", fontcolor=red];
   cc6_1_b [label="(cc 6 1)"];

   cc6_1_b -> cc6_0_b;
   cc6_1_b -> cc5_1_b;
   cc6_0_b [label="(cc 6 0)", fontcolor=red];
   cc5_1_b [label="(cc 5 1)"];

   cc5_1_b -> cc5_0_b;
   cc5_1_b -> cc4_1_b;
   cc5_0_b [label="(cc 5 0)", fontcolor=red];
   cc4_1_b [label="(cc 4 1)"];

   cc4_1_b -> cc4_0_b;
   cc4_1_b -> cc3_1_b;
   cc4_0_b [label="(cc 4 0)", fontcolor=red];
   cc3_1_b [label="(cc 3 1)"];

   cc3_1_b -> cc3_0_b;
   cc3_1_b -> cc2_1_b;
   cc3_0_b [label="(cc 3 0)", fontcolor=red];
   cc2_1_b [label="(cc 2 1)"];
   
   cc2_1_b -> cc2_0_b;
   cc2_1_b -> cc1_1_d;
   cc2_0_b [label="(cc 2 0)", fontcolor=red];
   cc1_1_d [label="(cc 1 1)"];   

   cc1_1_d -> cc1_0_d;
   cc1_1_d -> cc0_1_d;
   cc1_0_d [label="(cc 1 0)", fontcolor=red];
   cc0_1_d [label="(cc 0 1)", fontcolor=green];


1.15
~~~~

.. code-block:: scheme

   (define (cube x) (* x x x))
   (define (p x)
     (-
      (* 3 x)
      (* 4 (cube x))))
   (define (sine angle)
     (if (not (> (abs angle) 0.1))
         angle
         (p (sine (/ angle 3.0)))))

   (sine 12.15)
   (p (sine 4.05))
   (p (p (sine 1.35)))
   (p (p (p (sine 0.45))))
   (p (p (p (p (sine 0.15)))))
   (p (p (p (p (p (sine 0.05))))))

`p` is applied 5 times when `sine` is called with 12.15.

Since the "index" is being divided by 3, and steps and space are
proportional in this case, the growth for both space and steps is
:math:`\theta (\log_3 n)`.  This can also be expressed as
:math:`\theta (\frac{\log n}{\log 3})`, which means the same thing.

Becauase this will result in non-integer values, and steps must be in
integers, number of steps can be giving by the ceiling:
:math:`ceil(\log_3 n)`.


1.2.4 Exponentiation
--------------------

A recursive definition for exponentiation, takes :math:`\theta (n)`
space and steps

.. code-block:: scheme

   (define (expt-rec b n)
     (if (= n 0)
         1
         (* b (expt-rec b (- n 1)))))

An iterative definition, takes :math:`\theta (n)` steps
and :math:`\theta (1)` space

.. code-block:: scheme

   (define (expt-iter b n)
     (define (iter b counter product)
       (if (= counter 0)
           product
           (iter b
                 (- counter 1)
                 (* b product))))
     (iter b n 1))


Successive squaring requires very few steps, and requires
:math:`\theta (\log n)`.  Computing :math:`b^{2n}` requires only one
more step than :math:`b^n`, so the size of exponent possible to
compute doubles with every extra multiplication step.

.. code-block:: scheme

   (define (square x) (* x x))
   (define (even? x) (= (remainder n 2) 0))

   (define (fast-expt b n)
     (cond ((= n 0) 1)
           ((even? n) (square (fast-expt b (/ n 2))))
           (else (* b (fast-expt b (- n 1))))))

Exercises
---------

1.16
~~~~

A procedure that uses successive squaring, uses a logarithmic number
of steps, and is iterative

.. literalinclude:: src/exercises/ch1/ex-1.16.scm
   :language: scheme

This one knocked me out, and probably shouldn't have.  Got a workable
solution on my own, that fulfills the requirements, but is less
elegant than the solution found online.

Generally, odd exponents tripped me up.  What I missed was that `a`
didn't have to be a running product, just the right product at the
end.  I never thought of changing the value of `b` itself, which the
algorithm from the wiki depends on.

On the other hand, my version sets initial conditions (in a fairly
simple way) that allows the iteration to carry out the more
fundamental calculations, in the same number of steps as the wiki
version.

.. note::

   definition of an **invariant quantity** is a good way to think
   about iterative algorithms.  in this case, the value of
   :math:`ab^n` is kept constant through each iteration


1.17
~~~~

A multiplication version of the general algorithm found in exercise 1.16

.. literalinclude:: src/exercises/ch1/ex-1.17.scm
   :language: scheme
