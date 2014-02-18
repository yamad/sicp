#lang scribble/lp

@title{Generic arithmetic system}

@section{Exercise 2.86}

To use arbitrary numeric types as components of complex numbers, we
change the operators used in complex number functions from the
built-ins ('+, '-, and so on) to the generic operators ('add, 'sub,
and so on).

@chunk[<complex-with-numeric-types-procs>
(define (add-complex z1 z2)
  (make-from-real-imag (add (real-part z1) (real-part z2))
                       (add (imag-part z1) (imag-part z2))))
(define (sub-complex z1 z2)
  (make-from-real-imag (sub (real-part z1) (real-part z2))
                       (sub (imag-part z1) (imag-part z2))))
(define (mul-complex z1 z2)
  (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                     (add (angle z1) (angle z2))))
(define (div-complex z1 z2)
  (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                     (sub (angle z1) (angle z2))))
]

@chunk[<complex-with-numeric-types-rectangular>
(define (magnitude z)
  (sqroot (add (square (real-part z))
               (square (imag-part z)))))
(define (angle z)
  (arctan (imag-part z) (real-part z)))
(define (make-from-mag-ang r a)
  (cons (mul r (cosine a)) (mul r (sine a))))
]

@chunk[<complex-with-numeric-types-polar>
(define (real-part z)
  (mul (magnitude z) (cosine (angle z))))
(define (imag-part z)
  (mul (magnitude z) (sine (angle z))))
(define (make-from-real-imag x y)
  (cons (sqroot (add (square x) (square y)))
        (arctan y x)))
]

To support the trigonometric operations within the complex number
package, we need generic versions of the following operators:

@chunk[<trig-generic-operations>
(define (sine n) (apply-generic 'sine n))
(define (cosine n) (apply-generic 'cosine n))
(define (arctan x y) (apply-generic 'arctan x y))
(define (sqroot n) (apply-generic 'sqroot n))
]

and we implement them in the scheme-number and rational number
packages.

@chunk[<trig-scheme-number>
(put 'sine '(scheme-number)
     (lambda (x) (tag (sin x))))
(put 'cosine '(scheme-number)
     (lambda (x) (tag (cos x))))
(put 'arctan '(scheme-number scheme-number)
     (lambda (x y) (tag (atan x y))))
(put 'sqroot '(scheme-number)
     (lambda (x) (tag (sqrt x))))
]

@chunk[<trig-rational>
(put 'sine '(rational)
     (lambda (x)
       (sine (rational->real x))))
(put 'cosine '(rational)
     (lambda (x)
       (cosine (rational->real x))))
(put 'arctan '(rational rational)
     (lambda (x y)
       (arctan (rational->real x) (rational->real y))))
(put 'sqroot '(rational)
     (lambda (x)
       (make-rational (sqrt (numer x)) (sqrt (denom x)))))
]

We also need a generic @racket[square] operation. Because multiplication is already generic, this is easy:

@chunk[<square>
(define (square n) (mul n n))
]

@chunk[<tests-trig>
(test-suite
 "tests for generic trig functions"
 (check-= 0 (sine (make-scheme-number 0)) 1e-3)
 (check-= 1 (cosine (make-scheme-number 0)) 1e-3)
 (check arctan (make-scheme-number 1) (make-scheme-number 1))
 (check-equal? 4 (square (make-scheme-number 2)))
 (check-equal? 2 (sqroot (make-scheme-number 4)))

 (check-= 1.0 (sine (make-rational pi 2)) 1e-3)
 (check-= 0.0 (cosine (make-rational pi 2)) 1e-3)
 (check arctan (make-rational pi 2) (make-rational 1 2))
 (check-equal? (make-rational 1 4) (square (make-rational 1 2)))
 (check-equal? (make-rational 2 3) (sqroot (make-rational 4 9)))
 )
]

@chunk[<tests-internal-complex>
(test-suite
 "tests for using numeric types inside complex numbers"
 (check-equal? (make-complex-from-real-imag
                (make-rational 1 2)
                (make-rational 2 3))
               (add (make-complex-from-real-imag
                     (make-rational 1 4)
                     (make-rational 1 3))
                    (make-complex-from-real-imag
                     (make-rational 1 4)
                     (make-rational 1 3))))
 (check-equal? (make-complex-from-real-imag 1.7 2.7)
               (add (make-complex-from-real-imag 1.2 2.2)
                    (make-complex-from-real-imag
                     (make-rational 1 2)
                     (make-rational 1 2))))
 )
]

Now, I think due to the trigonometric functions, the result of some
operations is a rational number with very large numerator and
denominator. To deal with this, I changed @racket[make-rational] to
check how big the components are. If they are too big, a real number
is created instead.

@chunk[<make-rational>
(define (make-rational n d)
  (let ((n-len (int-len n))
        (d-len (int-len d)))
    (if (and (< n-len 4) (< d-len 4))
        ((get 'make 'rational) n d)
        (make-scheme-number (/ n d)))))
]

@chunk[<int-len>
(define (int-len n)
  (define (iter x acc)
    (if (= x 0) acc
        (iter (truncate (/ x 10))
              (+ 1 acc))))
  (iter n 0))
]

@section{Supporting code}

This is the generic arithmetic system from section 2.5 of SICP,
specified in a literate programming style using
@link["http://racket-lang.org"]{Racket's} literate programming
language @racket[scribble/lp]. This file can be run by a racket
compiler/interpreter directly without modification. To tranform this
file into LP documentation, use `raco scribble
generic-arithmetic.scrbl`.

The full system is composed as follows:

@chunk[<*>
       <arithmetic-api>
       <generic-dispatch-framework>
       <numbers-packages>
       <helper-fns>
       <tests>
       (provide (all-defined-out))
]

@subsection{External API}

@chunk[<arithmetic-api>
<external-constructors>
<generic-selectors>
<generic-operations>
]

@chunk[<external-constructors>
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))
<make-rational>
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))
]

@chunk[<generic-operations>
(define (add x y)  (apply-generic 'add x y))
(define (sub x y)  (apply-generic 'sub x y))
(define (mul x y)  (apply-generic 'mul x y))
(define (div x y)  (apply-generic 'div x y))
(define (equ? a b) (apply-generic 'equ? a b))
(define (=zero? a) (apply-generic '=zero? a))
(define (raise n)  (apply-generic 'raise n))
(define (tower-level n) (apply-generic 'tower-level n))
(define (project n) (apply-generic 'project n))
(define (drop n)
  (let ((project-proc (get 'project (list (type-tag n)))))
    (if project-proc
        (let ((projected-n (project-proc (contents n))))
          (cond ((equal? (tower-level n) LEVEL-LOWEST) projected-n)
                ((equ? (raise projected-n) n) (drop projected-n))
                (else n)))
        n)))
(define DROPPABLE '(add sub mul div sine cosine arctan sqroot))
<trig-generic-operations>
]

@chunk[<generic-selectors>
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
]

@subsection{Function dispatch}

The generic arithmetic system centers around a data-directed dispatch
framework that determines the appropriate function to apply given the
type of the arguments in a given function call. The available
functions are registered with the system by the packages in the
@racket[<numbers-packages>] chunk.

@chunk[<generic-dispatch-framework>
<table-framework>
<tagged-data-accessors>
<apply-generic>
]

@chunk[<apply-generic>
(define (apply-generic op . args)
  (define (no-method op type-tags)
    (error "No method for these types"
            (list op type-tags)))
  (define (raise-lowest args)
    (let ((min-level (apply min (map tower-level args))))
      (map (lambda (x)
             (if (eq? min-level (tower-level x))
                 (let ((raise-proc (get 'raise (list (type-tag x)))))
                   (if raise-proc
                       (raise-proc (contents x))
                       x))
                 x))
           args)))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (let ((res (apply proc (map contents args))))
            (if (memq op DROPPABLE)
                (drop res)
                res))
          (let* ((raised-args (raise-lowest args))
                 (raised-type-tags (map type-tag raised-args)))
            (if (not (equal? type-tags raised-type-tags))
                (apply apply-generic (cons op raised-args))
                (no-method op type-tags)))))))
]

@chunk[<tagged-data-accessors>
(define (attach-tag type-tag datum)
  (cond ((number? datum) datum)
        (else
         (cons type-tag datum))))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else
         (error "Bad tagged datum: TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else
         (error "Bad tagged datum: CONTENTS" datum))))
]

@subsection{Numbers packages}

Each numeric type has a package of functions which are installed into
the global operations table.

@chunk[<numbers-packages>
<tower-level-constants>
<scheme-number-package>
<rational-number-package>
<complex-number-package>
<install-number-packages>
]

@chunk[<tower-level-constants>
(define LEVEL-INTEGER  1)
(define LEVEL-RATIONAL 2)
(define LEVEL-REAL     3)
(define LEVEL-COMPLEX  4)
(define LEVEL-LOWEST   LEVEL-INTEGER)
(define LEVEL-HIGHEST  LEVEL-COMPLEX)
]

@chunk[<scheme-number-package>
(define (install-scheme-number-package)
  (define (tag x) x)
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'equ? '(scheme-number scheme-number)
       equal?)
  (put '=zero? '(scheme-number)
       (lambda (x) (equal? 0 x)))
  <trig-scheme-number>

  ;; coercion and raise functions
  (define (integer->rational n)
    (make-rational n 1))
  (define (real->complex n)
    (make-complex-from-real-imag n 0))
  (put 'raise '(scheme-number)
       (lambda (x)
         (if (exact-integer? x)
             (integer->rational x)
             (real->complex x))))
  (define (real->rational n)
    (let ((r (inexact->exact n)))
      (make-rational (numerator r)
                     (denominator r))))
  (put 'project '(scheme-number)
       (lambda (x)
         (if (exact-integer? x) x
             (real->rational x))))

  (put 'tower-level '(scheme-number)
       (lambda (x)
         (if (exact-integer? x)
             LEVEL-INTEGER
             LEVEL-REAL)))
  )
]

@chunk[<rational-number-package>
(define (install-rational-package)
;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))

  ;; interface to rest of system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
        (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
        (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
        (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
        (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
        (lambda (n d) (tag (make-rat n d))))
  (put 'equ? '(rational rational)
       (lambda (a b)
         (and (equal? (numer a) (numer b))
              (equal? (denom a) (denom b)))))
  (put '=zero? '(rational)
       (lambda (x)
         (equal? 0 (numer x))))
  <trig-rational>

  ;; coercion functions
  (define (rational->real n)
    (make-scheme-number
     (exact->inexact (/ (numer n) (denom n)))))
  (put 'raise '(rational) (lambda (x) (rational->real x)))
  (put 'project '(rational)
     (lambda (x) (make-scheme-number (numer x))))

  (put 'tower-level '(rational)
       (lambda (x) LEVEL-RATIONAL))
  )
]

@chunk[<complex-number-package>
(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  ;; internal procedures
  <complex-with-numeric-types-procs>

  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
        (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
        (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
        (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
        (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
        (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
        (lambda (r a) (tag (make-from-mag-ang r a))))

  (define (equ? a b)
  (and (equal? (exact-if-possible (real-part a))
               (exact-if-possible (real-part b)))
       (equal? (exact-if-possible (imag-part a))
               (exact-if-possible (imag-part b)))))
  (put 'equ? '(complex complex) equ?)

  (put '=zero? '(complex)
       (lambda (x)
         (equal? 0 (magnitude x))))

  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'tower-level '(complex)
       (lambda (x) LEVEL-COMPLEX))
  (put 'project '(complex)
     (lambda (x) (make-scheme-number (real-part x))))

  <complex-rectangular-package>
  <complex-polar-package>
  (install-polar-package)
  (install-rectangular-package)
  )
]

@chunk[<complex-rectangular-package>
  (define (install-rectangular-package)
    ;; internal procedures
    (define (real-part z) (car z))
    (define (imag-part z) (cdr z))
    (define (make-from-real-imag x y) (cons x y))
    <complex-with-numeric-types-rectangular>

    ;; interface to rest of system
    (define (tag x) (attach-tag 'rectangular x))
    (put 'real-part '(rectangular) real-part)
    (put 'imag-part '(rectangular) imag-part)
    (put 'magnitude '(rectangular) magnitude)
    (put 'angle '(rectangular) angle)
    (put 'make-from-real-imag 'rectangular
         (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'rectangular
         (lambda (r a) (tag (make-from-mag-ang r a))))
    (put 'tower-level '(rectangular)
         (lambda (x) LEVEL-COMPLEX))
    )
]

@chunk[<complex-polar-package>
  (define (install-polar-package)
    ;; internal procedures
    (define (magnitude z) (car z))
    (define (angle z) (cdr z))
    (define (make-from-mag-ang r a) (cons r a))
    <complex-with-numeric-types-polar>

    ;; interface to rest of system
    (define (tag x) (attach-tag 'polar x))
    (put 'real-part '(polar) real-part)
    (put 'imag-part '(polar) imag-part)
    (put 'magnitude '(polar) magnitude)
    (put 'angle '(polar) angle)
    (put 'make-from-real-imag 'polar
         (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'polar
         (lambda (r a) (tag (make-from-mag-ang r a))))
    (put 'tower-level '(polar)
         (lambda (x) LEVEL-COMPLEX))
    )
]

@chunk[<install-number-packages>
(install-scheme-number-package)
(install-rational-package)
(install-complex-package)
]

@subsection{Data-directed table framework}

To support the data-directed design, we need a way to store the table
of functions keyed on argument type. SICP does not provide an
implementation of these functions (until later?), so we will have to
build a table storage framework ourselves to test our code.

@chunk[<table-framework>
;; return table with key-value pair removed by key
(define (prune-key key table)
  (define (remove-kv key table)
    (cond ((null? table) table)
          ((equal? key (caar table))
           (cdr table))
          (else
           (cons (car table) (remove-kv key (cdr table))))))
  (if (member key (map car table))
      (remove-kv key table)
      table))

(define (put op type item)
  (let ((key (make-op-key op type)))
    (set! op-table (cons (cons key item)
                         (prune-key key op-table)))))

(define (get op type)
  (let ((match (assoc (make-op-key op type) op-table)))
    (if match (cdr match)
        #f)))

(define op-table '())
(define (make-op-key op type)
  (list op type))
]

@subsection{Miscellaneous}

Some helper functions from previous chapters are needed to make
everything work.

@chunk[<helper-fns>
(define (exact-if-possible n)
  (if (integer? n)
      (inexact->exact n)
      n))
<square>
<int-len>
]

@section{Tests}

@chunk[<tests>
(require rackunit)
(require rackunit/text-ui)
(require racket/math)

(define sn-a (make-scheme-number 1))
(define sn-b (make-scheme-number 2))

(define rat-a (make-rational 1 4))
(define rat-b (make-rational 2 3))

(define rect-a (make-complex-from-real-imag 1 1))
(define rect-b (make-complex-from-real-imag 2 3))

(define polar-a (make-complex-from-mag-ang 1 2))
(define polar-b (make-complex-from-mag-ang 2 4))
(define all-tests
  (test-suite
   "All arithmetic system tests"
   <tests-scheme-number>
   <tests-rational>
   <tests-complex-rectangular>
   <tests-complex-polar>
   <tests-equ?-op>
   <tests-zero?-op>
   <tests-coercions>
   <tests-raise>
   <tests-tower-level>
   <tests-project>
   <tests-drop>
   <tests-internal-complex>
   <tests-trig>
   ))
(run-tests all-tests)
]

@chunk[<tests-project>
(test-suite
 "tests for project operator"
 (check-equal? 2 (project (make-rational 2 1)))
 (check-equal? 1 (project (make-rational 2 2)))
 (check-equal? 1 (project (make-scheme-number 1)))
 (check-equal? 2 (project (make-complex-from-mag-ang 2 0)))
 (check-equal? 2.0 (project (make-complex-from-real-imag 2.0 0)))
 )
]

@chunk[<tests-drop>
(test-suite
 "tests for drop operator"
 (check-equal? 1 (drop 1))
 (check-equal? 1 (drop 1.0))
 (check-equal? 2 (drop (make-rational 2 1)))
 (check-equal? 2 (drop (make-rational 2.0 1)))
 (check-equal? 2 (drop (make-rational 4 2)))
 (check-equal? (make-rational 4 3)
               (drop (make-rational 4 3)))
 (check-equal? 1 (drop (make-complex-from-real-imag 1 0)))
 (check-equal? 1 (drop (make-complex-from-real-imag 1.0 0)))
 )
]

@chunk[<tests-tower-level>
(test-suite
 "tests for tower-level operator"
 (check-equal? LEVEL-INTEGER (tower-level 1))
 (check-equal? LEVEL-RATIONAL (tower-level (make-rational 2 1)))
 (check-equal? LEVEL-REAL (tower-level 1.0))
 (check-equal? LEVEL-COMPLEX (tower-level (make-complex-from-mag-ang 1 1)))
 (check-equal? LEVEL-COMPLEX (tower-level (make-complex-from-real-imag 1 1)))
 )
]

@chunk[<tests-raise>
(test-suite
 "tests for raise operation"
 (check-equal? (make-rational 2 1)
               (raise (make-scheme-number 2)))
 (check-equal? (make-complex-from-real-imag 2.5 0)
               (raise (make-scheme-number 2.5)))
 (check-equal? (make-scheme-number 0.75)
               (raise (make-rational 3 4)))
 (check-equal? (make-scheme-number 2.0)
               (raise (make-rational 4 2)))
 (check-equal? (make-scheme-number 2.0)
               (raise (raise (make-scheme-number 2))))
 (check-equal? (make-complex-from-real-imag 2.0 0)
               (raise (raise (raise (make-scheme-number 2)))))
 )
]

@chunk[<tests-coercions>
(test-suite
 "coercion tests"
 (check-equal? (make-complex-from-real-imag 2 1)
               (add sn-a rect-a))
 (check-equal? (add sn-a rect-a)
               (add rect-a sn-a))
 )
]

@chunk[<tests-equ?-op>
(test-suite
 "tests for equ? operator"
 (check-true  (equ? sn-a sn-a))
 (check-false (equ? sn-a sn-b))
 (check-true  (equ? rat-a rat-a))
 (check-false (equ? rat-a rat-b))
 (check-true  (equ? rect-a rect-a))
 (check-false (equ? rect-a rect-b))
 (check-true  (equ? polar-a polar-a))
 (check-false (equ? polar-a polar-b))

 (check-true (equ? 3 (make-rational 3 1)))
 (check-true (equ? 3.0 (make-complex-from-real-imag 3.0 0)))
 (check-true (equ? (make-rational 2 1) (make-complex-from-real-imag 2 0)))
 )
]

@chunk[<tests-zero?-op>
(test-suite
 "tests for the =zero? operator"
 (check-true  (=zero? (make-scheme-number 0)))
 (check-false (=zero? (make-scheme-number 1)))

 (check-true  (=zero? (make-rational 0 1)))
 (check-true  (=zero? (make-rational 0 2)))
 (check-false (=zero? (make-rational 1 0)))
 (check-false (=zero? (make-rational 1 2)))

 (check-true  (=zero? (make-complex-from-real-imag 0 0)))
 (check-false (=zero? (make-complex-from-real-imag 0 1)))
 (check-false (=zero? (make-complex-from-real-imag 1 0)))
 (check-false (=zero? (make-complex-from-real-imag 1 1)))

 (check-true  (=zero? (make-complex-from-mag-ang 0 0)))
 (check-true  (=zero? (make-complex-from-mag-ang 0 100)))
 (check-false (=zero? (make-complex-from-mag-ang 1 0)))
 )
]

@chunk[<tests-scheme-number>
(test-suite
 "scheme number generic operations test"
 (let ((a (make-scheme-number 1))
       (b (make-scheme-number 2))
       (add-id (make-scheme-number 0))
       (mul-id (make-scheme-number 1)))
   (check-equal? (make-scheme-number 3) (add a b))
   (check-equal? (add a b) (add b a))
   (check-equal? a (add a add-id))

   (check-equal? (make-scheme-number 2) (mul a b))
   (check-equal? (mul a b) (mul b a))
   (check-equal? (make-scheme-number 4) (mul b b))
   (check-equal? a (mul a mul-id))

   (check-equal? (make-scheme-number 1) (sub b a))
   (check-equal? (make-scheme-number -1) (sub a b))

   (check-equal? (make-scheme-number 2) (div b a))

   ;; scheme numbers are untagged native numbers
   (check-equal? 2 (make-scheme-number 2))
   (check-equal? 0.7 (make-scheme-number 0.7))
   ))
]

@chunk[<tests-rational>
(test-suite
 "rational number generic operations tests"
 (let ((a (make-rational 1 4))
       (b (make-rational 2 3))
       (add-id (make-rational 0 1)))
   (check-equal? (make-rational 11 12)
                 (add a b))
   (check-equal? (add a b) (add b a))
   (check-equal? a (add a add-id))

   (check-equal? (make-rational 2 12)
                 (mul a b))
   (check-equal? (mul a b) (mul b a))

   (check-equal? (make-rational 5 12)
                 (sub b a))
   (check-equal? (make-rational -5 12)
                 (sub a b))

   (check-equal? (make-rational 3 8) (div a b))
   (check-equal? (make-rational 8 3) (div b a))
   ))
]

@chunk[<tests-complex-rectangular>
(test-suite
 "tests for complex rectangular basic ops"
 (let ((a (make-complex-from-real-imag 1 1))
       (b (make-complex-from-real-imag 2 3))
       (add-id (make-complex-from-real-imag 0 0)))

   (check-equal? 2 (real-part b))
   (check-equal? 3 (imag-part b))
   (check-= 1.414 (magnitude a) 1e-3)
   (check-= 0.785 (angle a) 1e-3)

   (check-equal? (make-complex-from-real-imag 3 4)
                 (add a b))
   (check-equal? (add a b) (add b a))
   (check-equal? a (add a add-id))

   (check-equal? (make-complex-from-real-imag 1 2)
                 (sub b a))
 ))
]

@chunk[<tests-complex-polar>
(test-suite
 "tests for complex polar basic ops"
 (let ((a (make-complex-from-mag-ang 1 2))
       (b (make-complex-from-mag-ang 2 4)))

   (check-equal? 2 (magnitude b))
   (check-equal? 4 (angle b))
   (check-= -1.307 (real-part b) 1e-3)
   (check-= -1.513 (imag-part b) 1e-3)

   (check-equal? (make-complex-from-mag-ang 2 6)
                 (mul a b))
   (check-equal? (make-complex-from-mag-ang 4 8)
                 (mul b b))
   (check-equal? (make-complex-from-mag-ang 2 2)
                 (div b a))
 ))
]
