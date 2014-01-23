#lang scribble/lp
@#reader scribble/comment-reader

@title{Generic arithmetic system}

This is the generic arithmetic system from section 2.4 of SICP,
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
       ]

@section{External API}

The system exposes constructors for number objects and operations on
such objects as an API.

@chunk[<arithmetic-api>
<external-constructors>
<generic-operations>
]

To build numbers appropriate for use within the arithmetic system, the
following constructors are available:

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

Similarly, the following operations are available. The system "does
the right thing" with different types of numbers when using these
generic operators. The generality is acheived by calling the
appropriate function via @racket[apply-generic].

@chunk[<generic-operations>
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (exp x y) (apply-generic 'exp x y))
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
(define (equ? a b) (apply-generic 'equ? a b))
(define (=zero? z) (apply-generic '=zero? z))
]

@section{Function dispatch}

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
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion (list type1 type2)))
                      (t2->t1 (get-coercion (list type2 type1))))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else
                         (error "No method for these types"
                                (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))
]

Data within the system is tagged with its type. The following
functions provide a common facility for building and accessing tagged
data.

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

@section{Numbers packages}

Each numeric type has a package of functions which are installed into the global operations table.

@chunk[<numbers-packages>
<scheme-number-package>
<rational-number-package>
<complex-number-package>
<number-coercions>
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
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (equal? x y)))
  (put 'exp '(scheme-number scheme-number)
       (lambda (x y) (expt x y)))
  (put 'make 'scheme-number (lambda (x) (tag x)))
  (put '=zero? 'scheme-number (lambda (x) (equal? x 0)))
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
  (define (equ? x y)
    (and (equal? (numer x) (numer y))
         (equal? (denom x) (denom y))))
  (define (=zero? z)
    (equal? (numer z) 0))

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
  (put 'equ? '(rational rational) equ?)
  (put '=zero? '(rational) =zero?)
  (put 'make 'rational
        (lambda (n d) (tag (make-rat n d))))
  )
]

Note that the complex number package holds within itself two other
packages that handle rectangular and polar representations of complex
numbers respectively.

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
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put '=zero? '(complex) =zero?)
  (put 'equ? '(complex complex) equ?)

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
    (define (equ? a b)
      (and (equal? (real-part a) (real-part b))
           (equal? (imag-part a) (imag-part b))))
    (define (=zero? z)
      (and (equal? (real-part z) 0)
           (equal? (imag-part z) 0)))
    (define (make-from-mag-ang r a)
      (cons (* r (cos a)) (* r (sin a))))

    ;; interface to rest of system
    (define (tag x) (attach-tag 'rectangular x))
    (put 'real-part '(rectangular) real-part)
    (put 'imag-part '(rectangular) imag-part)
    (put 'magnitude '(rectangular) magnitude)
    (put 'angle '(rectangular) angle)
    (put 'equ? '(rectangular rectangular) equ?)
    (put '=zero? '(rectangular) =zero?)
    (put 'make-from-real-imag 'rectangular
         (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'rectangular
         (lambda (r a) (tag (make-from-mag-ang r a))))
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
    (define (equ? a b)
      (and (equal? (magnitude a) (magnitude b))
           (equal? (angle a) (angle b))))
    (define (=zero? z)
      (and (equal? (magnitude z) 0)
           (equal? (angle z) 0)))
    (define (make-from-real-imag x y)
      (cons (sqrt (+ (square x) (square y)))
            (atan y x)))

    ;; interface to rest of system
    (define (tag x) (attach-tag 'polar x))
    (put 'real-part '(polar) real-part)
    (put 'imag-part '(polar) imag-part)
    (put 'magnitude '(polar) magnitude)
    (put 'angle '(polar) angle)
    (put 'equ? '(polar polar) equ?)
    (put '=zero? '(polar) =zero?)
    (put 'make-from-real-imag 'polar
         (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'polar
         (lambda (r a) (tag (make-from-mag-ang r a))))
    )
]

@chunk[<install-number-packages>
(install-scheme-number-package)
(install-rational-package)
(install-complex-package)
]

@chunk[<number-coercions>
(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))
(put-coercion '(scheme-number complex) scheme-number->complex)
]

@section{Data-directed table framework}

To support the data-directed design, we need a way to store the table
of functions keyed on argument type. SICP does not provide an
implementation of these functions (until later?), so we will have to
build a table storage framework ourselves to test our code.

@chunk[<table-framework>
       <key-table-framework>
       <coercion-table-framework>
       ]

First, we define @racket[put] and @racket[get], which store and
retrieve keyed entries in a global table. The table is a simple list,
but client code uses only @racket[put] and @racket[get] to interact
with it.

@chunk[<key-table-framework>
(define (put op type item)

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

  (let ((key (make-op-key op type)))
    (set! op-table (cons (cons key item)
                         (prune-key key op-table)))))

(define (get op type)
  (let ((match (assoc (make-op-key op type) op-table)))
    (if match (cdr match)
        (error "get error: no entry for " op type))))

(define op-table '())
(define (make-op-key op type)
  (list op type))
]

A similar table holds the functions for coercion of arguments to
different types.

@chunk[<coercion-table-framework>
(define coercion-table '())
(define (put-coercion type item)
  (set! coercion-table (cons type item)))
(define (get-coercion type)
  (let ((match (assoc type coercion-table)))
    (if match (cdr match)
        (error "get-coercion error: no entry for " type))))
]

@section{Miscellaneous}

Some helper functions from previous chapters are needed to make
everything work.

@chunk[<helper-fns>
(define (square x) (* x x))
]


@section{Tests}

@chunk[<tests>
(require racket/unit)
]
