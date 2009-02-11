.. Structure and Interpretation of Computer Programs Notes
.. :Author: Jason Yamada-Hanff

================================================
Chapter 1. Building Abstractions with Procedures
================================================
:Started: 2008-02-03


1.1 Elements of Programming
===========================

Three main mechanisms for combining ideas:

 primitive expressions : simple entities
 means of combination : build compound elements out of simple ones
 means of abstraction : naming of compound elements

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
