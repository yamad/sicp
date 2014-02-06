#lang scribble/lp

@title{Generic arithmetic system}

@section{Exercise 2.84}

First, we define the numeric tower heirarchy as a series of constants,
with higher numbers meaning a more "advanced" place in the heirarchy.

@chunk[<tower-level-constants>
(define LEVEL-INTEGER  1)
(define LEVEL-RATIONAL 2)
(define LEVEL-REAL     3)
(define LEVEL-COMPLEX  4)
(define LEVEL-LOWEST   LEVEL-INTEGER)
(define LEVEL-HIGHEST  LEVEL-COMPLEX)
]

These values are returned by the generic operator
@racket[tower-level], which is implemented for each numeric type.

@chunk[<tower-level-op>
(define (tower-level x) (apply-generic 'tower-level x))
]

@chunk[<tower-level-scheme-number>
(put 'tower-level '(scheme-number)
     (lambda (x)
       (if (exact-integer? x)
           LEVEL-INTEGER
           LEVEL-REAL)))
]

@chunk[<tower-level-rational>
(put 'tower-level '(rational)
     (lambda (x) LEVEL-RATIONAL))
]

@chunk[<tower-level-complex>
(put 'tower-level '(complex)
     (lambda (x) LEVEL-COMPLEX))
]

@chunk[<tower-level-rectangular>
(put 'tower-level '(complex)
     (lambda (x) LEVEL-COMPLEX))
]

@chunk[<tower-level-polar>
(put 'tower-level '(complex)
     (lambda (x) LEVEL-COMPLEX))
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

This version of @racket[apply-generic] works on any number of
arguments in any order and coerces those arbitrary arguments as far as
possible.

Note that the internal function @racket[raise-lowest] requires that
every tagged data type must implement @racket[tower-level], so
rectangular and polar subpackages also need an implementation.

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
          (apply proc (map contents args))
          (let* ((raised-args (raise-lowest args))
                 (raised-type-tags (map type-tag raised-args)))
            (if (not (equal? type-tags raised-type-tags))
                (apply apply-generic (cons op raised-args))
                (no-method op type-tags)))))))
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
       <tower-level-constants>
       <arithmetic-api>
       <generic-dispatch-framework>
       <numbers-packages>
       <helper-fns>
       <tests>
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
(define (make-rational n d)
  ((get 'make 'rational) n d))
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
<tower-level-op>
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
<scheme-number-package>
<rational-number-package>
<complex-number-package>
<install-number-packages>
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

  <tower-level-scheme-number>
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

  ;; coercion functions
  (define (rational->real n)
    (make-scheme-number
     (exact->inexact (/ (numer n) (denom n)))))
  (put 'raise '(rational) (lambda (x) (rational->real x)))

  <tower-level-rational>
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
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))

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
  (put 'equ? '(complex complex) equ?)
  (put '=zero? '(complex)
       (lambda (x)
         (equal? 0 (magnitude x))))

  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  <tower-level-complex>

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
    (define (magnitude z)
      (sqrt (+ (square (real-part z))
               (square (imag-part z)))))
    (define (angle z)
      (atan (imag-part z) (real-part z)))
    (define (make-from-mag-ang r a)
      (cons (* r (cos a)) (* r (sin a))))
    (define (equ? a b)
      (and (equal? (real-part a) (real-part b))
           (equal? (imag-part a) (imag-part b))))

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
    (put 'equ? '(rectangular rectangular) equ?)
    <tower-level-rectangular>
    )
]

@chunk[<complex-polar-package>
  (define (install-polar-package)
    ;; internal procedures
    (define (magnitude z) (car z))
    (define (angle z) (cdr z))
    (define (make-from-mag-ang r a) (cons r a))
    (define (real-part z)
      (* (magnitude z) (cos (angle z))))
    (define (imag-part z)
      (* (magnitude z) (sin (angle z))))
    (define (make-from-real-imag x y)
      (cons (sqrt (+ (square x) (square y)))
            (atan y x)))
    (define (equ? a b)
      (and (equal? (magnitude a) (magnitude b))
           (equal? (angle a) (angle b))))

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
    (put 'equ? '(polar polar) equ?)
    <tower-level-polar>
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
(define (square x) (* x x))
]

@section{Tests}

@chunk[<tests>
(require rackunit)
(require rackunit/text-ui)

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
   ))
(run-tests all-tests)
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
 (check-equal? (make-complex-from-real-imag 2.0 1)
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
