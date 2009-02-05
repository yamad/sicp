.. Structure and Interpretation of Computer Programs Notes
.. :Author: Jason Yamada-Hanff

Chapter 1. Building Abstractions with Procedures
================================================
:Started: 2008-02-03

Three main mechanisms for combining ideas:

 primitive expressions : simple entities
 means of combination : build compound elements out of simple ones
 means of abstraction : naming of compound elements

Two kinds of elements in programming:

 * procedures
 * data

Important aspects of lisp syntax:

.. code-block:: scheme

   ;; prefix notation
   > (+ 3 4)
   7
   > (+ (* 2 2) (/ 4 2))
   6

   ;; naming with `define`
   > (define size 2)
   > size
   2


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

ex. 1.5 

.. math::

   \frac{5+4+(2-(3-(6+\frac{4}{5})))}{3(6-2)(2-7)}
