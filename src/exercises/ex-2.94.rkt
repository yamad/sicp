#lang scribble/lp

@title{Generic arithmetic system}

@section{Exercise 2.94}

@chunk[<install-gcd-poly>
(define (gcd-terms a b)
  (if (empty-termlist? b)
      a
      (gcd-terms b (remainder-terms a b))))

(define (remainder-terms a b)
  (cadr (div-terms a b)))

(define (gcd-poly a b)
  (let* ((lv1 (list-variables a))
         (lv2 (list-variables b))
         (lv (car (sort (set-union lv1 lv2) compare-variables))))
    (make-poly lv (gcd-terms (term-list a)
                             (term-list b)))))
(put 'greatest-common-divisor '(polynomial polynomial)
     (lambda (a b) (tag (gcd-poly a b))))
(trace gcd-terms)
]

@chunk[<install-gcd-number>
(define (gcd-num a b)
  (if (= b 0)
      a
      (gcd-num b (remainder a b))))
(put 'greatest-common-divisor '(scheme-number scheme-number) gcd)
]

@chunk[<tests-gcd>
(test-suite
 "greatest-common-divisor tests"
 (let ((p1 (make-polynomial-sparse 'x '((4 1) (3 -1) (2 -2) (1 2))))
       (p2 (make-polynomial-sparse 'x '((3 1) (1 -1)))))
   (check-equal? (make-polynomial-sparse 'x '((2 -1) (1 1)))
                 (greatest-common-divisor p1 p2)))
 (check-equal? 3 (greatest-common-divisor 12 9))
 (check-equal? 7 (greatest-common-divisor 7 7)))
]

@section{Exercise 2.93}

"Modify the rational arithmetic package to use generic operations, but
change make-rat so that is does not attempt to reduce fractions to
lowest terms..."

First, replace the primitive operators with the generic operators for
the package:

@chunk[<internal-rational-operations>
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
]

and remove the gcd function on make-rat:

@chunk[<internal-make-rat>
(define (make-rat n d)
  (let ((g (greatest-common-divisor n d)))
    (cons (div n g) (div d g))))

]

This makes 3 of the tests for GCD function fail. No worries, it should
be fixed by the end of this series of exerices.

The automatic coercion system chokes on the embedded polynomials. This
is because much of the handling of rationals assumes that they can be
converted into real numbers. We could modify the rationals package,
but we won't for now. The assumptions of the type hierarchy run
relatively deep.

Instead, we add rational polynomial functions as a new type

@chunk[<type-tower-with-ratpoly>
(define type-tower-main '(integer rational real complex polynomial rational-poly))
]
@chunk[<tower-ratpoly>
(define LEVEL-RATPOLY  (type-tower-main-level 'rational-poly))
]

The underlying coercion functions (raising/projecting) then also
change to handle rational functions and normal numerical rationals
differently:

@chunk[<rational-coercion-with-ratpoly>
  (define (internal-types x)
    (map type-tag (list (numer x) (denom x))))
  (define (rational-function? x)
    (equal? (internal-types x) '(polynomial polynomial)))
  (put 'raise '(rational)
       (lambda (x)
         (let ((tags (internal-types x)))
           (cond ((equal? tags '(scheme-number scheme-number))
                  (rational->real x))
                 ((equal? tags '(polynomial polynomial)) x)
                 (else x)))))
  (put 'project '(rational)
       (lambda (x)
         (if (rational-function? x) x
             (make-scheme-number (numer x)))))

  (put 'tower-level '(rational)
       (lambda (x)
         (cond ((rational-function? x) LEVEL-RATPOLY)
             (else LEVEL-RATIONAL))))
]

and we kludge the drop function so that rational functions are left alone.

@chunk[<drop-with-ratpoly-check>
(define (drop n)
  (let ((project-proc (get 'project (list (type-tag n)))))
    (if project-proc
        (let ((projected-n (project-proc (contents n))))
          (cond ((equal? (tower-level n) LEVEL-LOWEST) projected-n)
                ((equal? (tower-level n) LEVEL-RATPOLY) n)
                ((equ? (raise projected-n) n) (drop projected-n))
                (else n)))
        n)))
]

A more refined approach would probably build out separate subpackages
within the rational package to deal with normal rationals and rational
functions separately. Oh well, code rot already...

Using the test specified in the exercise, we confirm that rational
functions now "work" but that they are not in lowest terms:

@chunk[<tests-rational-functions>
(test-suite
 "rational function (polynomial arguments) tests"
 (let ((p1 (make-polynomial-sparse 'x '((2 1) (0 1))))
       (p2 (make-polynomial-sparse 'x '((3 1) (0 1)))))
   (define rf (make-rational p2 p1))
   (check-equal? '(rational (polynomial x sparse (5 2) (3 2) (2 2) (0 2))
                            polynomial x sparse (4 1) (2 2) (0 1))
                 (add rf rf))))
]

@section{Exercise 2.92}

"By imposing an ordering on variables, extend the polynomial package
so that addition and multiplication of polynomials works for
polynomials in different variables. (This is not easy!)"

True, not easy. This implementation uses an intermediate extended
representation to "unroll" (or, more mathematically, distribute) the
polynomials. The polynomials are then transformed into a canonical
form such that all variables are represent in order in each
polynomial. By using a canonical polynomial form, no code for the
actual operations has to change.

The current system is relatively slow because many transformations are
repeated over and over as numbers are interconverted to different
types more than they need to be. Perhaps I will root out these
inefficiences at some point.

@chunk[<poly-expand-to-terms>
(define (list-variables p)
  (define (termlist-vars tl)
    (cond ((empty-termlist? tl) '())
          ((equal? (type-tag (coeff (first-term tl))) 'polynomial)
           (append (list-variables (contents (coeff (first-term tl))))
                   (termlist-vars (rest-terms tl))))
          (else
           (termlist-vars (rest-terms tl)))))
  (let ((v (variable p))
        (tl (term-list p)))
    (if (null? tl) '()
        (sort (unique (cons v (termlist-vars tl))) compare-variables))))

(define (poly-expand-to-terms p)
  (define (ex-term var order)
    (list var order))
  (define (add-ex-term term tl)
    (define (adjoin-ex-term term tl)
      (if (null? tl) (list term)
          (let* ((ft (car tl))
                 (vft (car ft))
                 (oft (cadr ft))
                 (vt (car term))
                 (ot (cadr term)))
            (if (same-variable? vft vt)
                (cons (ex-term vft
                               (add oft ot))
                      (cdr tl))
                (cons (car tl)
                      (adjoin-ex-term term (cdr tl)))))))

    (if (null? tl) '()
        (let* ((ft (car tl))
               (coeff (car ft))
               (eterms (cdr ft)))
          (cons (cons coeff (adjoin-ex-term term eterms))
                (add-ex-term term (cdr tl))))))
  (define (poly-expand-help var tlist)
    (if (empty-termlist? tlist) '()
        (let* ((ft (first-term tlist))
               (c  (coeff ft))
               (o  (order ft)))
          (cond ((equal? (type-tag c) 'polynomial)
                 (append
                  (add-ex-term (ex-term var o)
                               (poly-expand-to-terms (contents c)))
                  (poly-expand-help var (rest-terms tlist))))
                (else
                 (cons
                  (cons c
                        (list (ex-term var o)))
                  (poly-expand-help var (rest-terms tlist))))))))
;  (trace poly-expand-to-terms)
  (define (poly-expand-sort p)
    (sort
     (map (lambda (t) (cons (car t) (sort (cdr t) compare-expand-terms))) p)
     #:key cadr
     compare-expand-terms))
  (let ((v (variable p))
        (tl (term-list p)))
    (poly-expand-sort (poly-expand-help v tl))))

(define (compare-expand-terms t1 t2)
  (cond ((string=? (symbol->string (car t1))
                   (symbol->string (car t2)))
         (> (cadr t1) (cadr t2)))
        (else
         (compare-variables (car t1) (car t2)))))

(define (poly->canon p lv)
  (define (ex->canon ep)
    (define (iter a acc)
      (if (null? a) acc
          (iter (cdr a)
                (add (car a) acc))))
;                (let ((r (add (car a) acc)))
;                  (if (not (constant? (contents r))) r
;                      (zero-order-coeff (term-list (contents r))))))))
    (define (ex-term->poly coeff terms)
      (if (null? terms)
          coeff
          (let* ((ft (car terms))
                 (var (car ft))
                 (order (cadr ft)))
            (make-polynomial-sparse
             var
             (list (list order
                         (ex-term->poly coeff (cdr terms))))))))
    (define (terms->polys etl)
      (map (lambda (t) (ex-term->poly (car t) (cdr t)))
           etl))

    (let ((pl (terms->polys ep)))
      (if (null? pl)
          (make-polynomial-sparse 'ANY-VARIABLE '())
          (iter (cdr pl) (car pl)))))

  (define (ex-terms-fill terms lv)
    (define (vars->zero-c-terms lv)
      (if (null? lv) '()
          (cons (list (car lv) 0)
                (vars->zero-c-terms (cdr lv)))))
    (cond
     ((null? lv) terms)
     ((null? terms) (vars->zero-c-terms lv))
     (else
      (let ((ft (car terms))
            (v (car lv)))
        (cond ((same-variable? (car ft) v)
               (cons ft (ex-terms-fill
                         (cdr terms) (cdr lv))))
              ((compare-variables (car ft) v)
               (cons ft (ex-terms-fill
                         (cdr terms) lv)))
              (else
               (cons (list v 0) (ex-terms-fill
                                 terms (cdr lv)))))))))

  (if (null? p) '()
      (let ((pe (map (lambda (t)
                       (cons (car t) (ex-terms-fill (cdr t) lv)))
                     (poly-expand-to-terms p))))
        (ex->canon pe))))
]

@chunk[<tests-polynomial-indeterminates>
  ;; test polynomials from JoT's Jottings
  ;; <http://jots-jottings.blogspot.com/2012/09/sicp-exercise-292-dealing-with.html>
  (let* ((p1
          (make-polynomial-dense
           'x
           (list
            5
            (make-polynomial-dense 'x '(10 6 4))
            3)))
         (p1-canon
          (make-polynomial-dense 'x '(10 11 4 3)))
         (p2
          (make-polynomial-dense 'x (list
                         (make-polynomial-dense 'y
                                    (list 10
                                          (make-polynomial-dense 'x '(10 6 4))
                                          4))
                         (make-polynomial-dense 'x '(10 6 4))
                         3)))
         (p2-canon
          (make-polynomial-dense
           'x
           (list
            (make-polynomial-dense 'y '(10 0))
            (make-polynomial-dense 'y '(6 10))
            (make-polynomial-dense 'y '(10 4 10))
            4 3)))
         (p3
          (make-polynomial-dense 'y
                     (list
                      (make-polynomial-dense 'x '(1 5 -3))
                      (make-polynomial-dense 'x '(2 3 1))
                      -5)))
         (p3-canon
          (make-polynomial-dense
           'x
           (list
            (make-polynomial-dense 'y '(1 2 0))
            (make-polynomial-dense 'y '(5 3 0))
            (make-polynomial-dense 'y '(-3 1 -5)))))
         (p4
          (make-polynomial-dense
           'x
           (list
            (make-polynomial-dense 'y '(5 2 -1))
            (make-polynomial-dense 'y '(2 1 2))
            -3)))
         (p4-canon p4)
         (p5
          (make-polynomial-dense
           'y
           (list
            (make-polynomial-dense 'x '(5 2 0))
            (make-polynomial-dense 'x '(2 1 0))
            (make-polynomial-dense 'x '(-1 2 -5)))))
         (p5-canon
          (make-polynomial-dense
           'x
           (list
            (make-polynomial-dense 'y '(5 2 -1))
            (make-polynomial-dense 'y '(2 1 2))
            -5)))
         (p6
          (make-polynomial-dense 'x '(42)))
         (p6-canon 42))

    (check-true (equ? p1-canon
                      (poly->canon (contents p1) (list-variables
                                                  (contents p1)))))
    (check-true (equ? p2-canon
                      (poly->canon (contents p2) (list-variables
                                                  (contents p2)))))
    (check-true (equ? p3-canon
                      (poly->canon (contents p3) (list-variables
                                                  (contents p3)))))
    (check-true (equ? p4-canon
                      (poly->canon (contents p4) (list-variables
                                                  (contents p4)))))
    (check-true (equ? p5-canon
                      (poly->canon (contents p5) (list-variables
                                                  (contents p5)))))
    (check-true (equ? p6-canon
                      (poly->canon (contents p6) (list-variables
                                                  (contents p6)))))
    (check-true (equ? (make-polynomial-dense
                       'x
                       (list
                        1
                        (make-polynomial-dense
                         'y
                         '(3 6))))
                      (add (make-polynomial-dense 'x '(1 2))
                           (make-polynomial-dense 'y '(3 4)))))
    )
]

@section{Exercise 2.91}

An implementation of @racket[div-terms] and @racket[div-poly]:

@chunk[<div-terms-rest-of-result>
(div-terms
 (sub-terms L1
            (mul-terms
             (make-sparse-list-from-terms (list (make-term new-o new-c)))
             L2))
 L2)
]

@chunk[<div-terms-form-complete-result>
(list (adjoin-term (make-term new-o new-c)
                   (car rest-of-result))
      (cadr rest-of-result))
]

@chunk[<div-terms>
(define (sub-terms L1 L2)
  (add-terms L1 (negate-terms L2)))

(define (div-terms L1 L2)
  (if (empty-termlist? L1)
      (list (the-empty-termlist L2)
            (the-empty-termlist L1))
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
        (if (> (order t2) (order t1))
            (list (the-empty-termlist L2) L1) ;
            (let ((new-c (div (coeff t1) (coeff t2)))
                  (new-o (sub (order t1) (order t2))))
              (let ((rest-of-result
                     <div-terms-rest-of-result>
                     ))
                <div-terms-form-complete-result>
                ))))))
]

@chunk[<div-poly>
(define (div-poly p1 p2)
  (let* ((final-type (select-termlist-type p1 p2))
         (lv1 (list-variables p1))
         (lv2 (list-variables p2))
         (lv (sort (set-union lv1 lv2) compare-variables))
         (cp1 (contents (poly->canon p1 lv)))
         (cp2 (contents (poly->canon p2 lv))))
    (map make-poly (list (car lv) (car lv))
         (div-terms (coerce final-type (term-list cp1))
                    (coerce final-type (term-list cp2))))))
]

@chunk[<install-div-poly>
(put 'div '(polynomial polynomial)
     (lambda (p1 p2)
       (map tag (div-poly p1 p2))))
]

@subsection{Polynomial Package}

@chunk[<make-polynomial>
(define (make-polynomial-sparse var terms)
  ((get 'make-sparse '(polynomial)) var terms))
(define (make-polynomial-dense var terms)
  ((get 'make-dense '(polynomial)) var terms))
]

@chunk[<generic-termlist-install>
(put 'make-dense  '(polynomial)
     (lambda (v tl) (tag (make-poly v (make-dense-termlist tl)))))
(put 'make-sparse  '(polynomial)
     (lambda (v tl) (tag (make-poly v (make-sparse-termlist tl)))))
(define (make-dense-termlist term-list)
  ((get 'make-termlist '(dense)) term-list))
(define (make-sparse-termlist term-list)
  ((get 'make-termlist '(sparse)) term-list))

(define (make-sparse-list-from-terms list-of-terms)
  ((get 'make-from-terms '(sparse)) list-of-terms))
(define (make-dense-list-from-terms list-of-terms)
  ((get 'make-from-terms '(dense)) list-of-terms))

<term-representation>
(install-term-package)

<termlist-api>
<generic-termlist-operations>
<termlist-type-tower>
<dense-term-representation>
<sparse-term-representation>
(install-dense-package)
(install-sparse-package)
]

@subsection{Term type}

The representation of terms is identical in every scope, so we create a
tagged type "term". (Introducing the term type is the major change
that allows most duplicate code to rise out of the individual term
list packages.)

@chunk[<term-representation>
(define (make-term order coeff)
  ((get 'make '(term)) order coeff))
(define (order term)
  (apply-generic 'order term))
(define (coeff term)
  (apply-generic 'coeff term))

(define (install-term-package)
  (define (tag t) (attach-tag 'term t))
  (define (make order coeff)
    (list order (drop coeff)))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (put 'make '(term) (lambda (o c) (tag (make o c))))
  (put 'order '(term) (lambda (t) (order t)))
  (put 'coeff '(term) (lambda (t) (coeff t)))
  )
]

The term type is used most importantly as an argument to
@racket[adjoin-term], where a term must be provided as the first
argument. This creates a slightly unusual case where we need to bind
adjoin-term to the term list first so that the generic operation knows
which term list representation to dispatch to. To deal with this, we
curry the function application. We introduce
@racket[apply-adjoin-term] which binds the term list and then returns
a single-argument function that takes a term to add to the term list.

@chunk[<adjoin-term-generic>
(define (adjoin-term term term-list)
  ((apply-generic 'apply-adjoin-term term-list) term))
]

@chunk[<dense-conversion>
(define (make-from-coeffs coeff-list)
  (map drop
       (strip-leading-zeros coeff-list)))
(define (make-from-terms raw-list)
  (if (null? raw-list)
      '()
      (adjoin-term (car raw-list)
                   (make-from-terms (cdr raw-list)))))
(define (to-terms term-list)
  (if (null? term-list) '()
      (cons (first-term term-list)
            (to-terms (rest-terms term-list)))))

(define (dense->sparse term-list)
  (make-sparse-list-from-terms (to-terms term-list)))

(put 'raise '(dense) (lambda (tl) (dense->sparse tl)))
(put 'make-from-terms '(dense) (lambda (tl) (tag (make-from-terms tl))))
(put 'coerce '(dense sparse) (lambda (tl) (dense->sparse tl)))
(put 'coerce '(dense dense) (lambda (tl) (tag tl)))
]

@chunk[<sparse-conversion>
(define (sparse-term-to-term term)
  (make-term (order-sparse term)
             (drop (coeff-sparse term))))
(define (to-terms term-list)
  (map sparse-term-to-term term-list))

(define (make-from-terms raw-list)
  (foldl adjoin-term '() raw-list))

(define (sparse->dense term-list)
  (make-dense-list-from-terms (to-terms term-list)))

(put 'raise '(sparse) (lambda (tl) (tag tl)))
(put 'make-from-terms '(sparse) (lambda (tl) (tag (make-from-terms tl))))
(put 'coerce '(sparse dense) (lambda (tl) (sparse->dense tl)))
(put 'coerce '(sparse sparse) (lambda (tl) (tag tl)))
]

We will use the @racket[coerce] function a little later. Stay tuned.

@subsection{Generic operations and interface}

The best change that results from defining a term type is that term
list operations like @racket[add-terms] and @racket[mul-terms] work
automatically in the polynomial package scope because they only use
functions from the term-list interface.

@chunk[<generic-termlist-operations>
(define (add-terms La Lb)
  (define (add-terms-internal L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms-internal (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms-internal L1 (rest-terms L2))))
                   (else
                    (adjoin-term (make-term (order t1) (add (coeff t1)
                                                            (coeff t2)))
                                 (add-terms-internal (rest-terms L1)
                                                     (rest-terms L2)))))))))
  (canonical-form (add-terms-internal La Lb)))

(define (mul-terms L1 L2)
  (if (empty-termlist? L1)
      (the-empty-termlist L1)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))

(define (mul-term-by-all-terms t1 L)
  (if (empty-termlist? L)
      (the-empty-termlist L)
      (let ((t2 (first-term L)))
        (adjoin-term
         (make-term (add (order t1) (order t2))
                    (mul (coeff t1) (coeff t2)))
         (mul-term-by-all-terms t1 (rest-terms L))))))

(define (negate-terms term-list)
  (if (empty-termlist? term-list)
      (the-empty-termlist term-list)
      (adjoin-term (make-term (order (first-term term-list))
                              (negate (coeff (first-term term-list))))
                   (negate-terms (rest-terms term-list)))))

<div-terms>
]

Each term list representation has to provide at least the following
functions as an interface for everything to work. Note that
@racket[the-empty-termlist] now takes an argument so that it knows
which term list representation it is working
with. @racket[canonical-form] ensures that the resulting term lists
are well formed (e.g. no zero coefficients where they don't carry
information)

@chunk[<termlist-api>
(define (first-term term-list)
  (apply-generic 'first-term term-list))
(define (rest-terms term-list)
  (apply-generic 'rest-terms term-list))
<adjoin-term-generic>
(define (empty-termlist? term-list)
  (apply-generic 'empty-termlist? term-list))
(define (the-empty-termlist term-list)
  (apply-generic 'the-empty-termlist term-list))
(define (zero-order-termlist? term-list)
  (apply-generic 'zero-order-termlist? term-list))
(define (zero-coeff-termlist? tl)
  (apply-generic 'zero-coeff-termlist? tl))
(define (canonical-form tl)
  (apply-generic 'canonical-form tl))
]

@subsection{Ordering polynomial variables}

By installing @racket[add-terms] and the like in polynomial scope, it
opens up the possiblity that the system would try to add (or multiply)
a dense and a sparse polynomial together. This works ok, but then the
type of the resulting polynomial is not well determined without a way
to favor one representation over the other. This is especially an
issue when the system upconverts a normal number to a polynomial with
an unbound (ANY-VARIABLE) variable. We definitely don't want to force
a certain term list representation from the system-created polynomial
in that case.

I decided the most sensible solution was to order the polynomials by
variable in alphabetical order ("x" beats "xa" beats "y"). The term
list representation for the "greatest" polynomial in the sort order is
the one used in the result.

@chunk[<polynomial-operations>
  (define (compare-variables v1 v2)
    (cond ((not v1) #f)
          ((not v2) #t)
          ((equal? v1 ANY-VARIABLE) #f)
          ((equal? v2 ANY-VARIABLE) #t)
          (else
           (string<? (symbol->string v1)
                     (symbol->string v2)))))
  (define (highest-variable var-list)
    (if (null? var-list)
        #f
        (let ((high-cdr (highest-variable (cdr var-list))))
          (if (compare-variables (car var-list) high-cdr)
              (car var-list)
              high-cdr))))
  (define (select-variable v1 v2)
    (highest-variable (list v1 v2)))
  (define (select-greater-poly p1 p2)
    (let* ((v1 (variable p1))
           (v2 (variable p2))
           (high-var (select-variable v1 v2)))
      (if (equal? v1 high-var) p1 p2)))
  (define (select-termlist-type p1 p2)
    (type-tag (term-list (select-greater-poly p1 p2))))

  (define (coerce type tl)
    (if (equal? type (type-tag tl))
        tl
        ((get 'coerce (list (type-tag tl) type)) (contents tl))))
  (define (op-poly f p1 p2 msg)
    (let* ((final-type (select-termlist-type p1 p2))
           (lv1 (list-variables p1))
           (lv2 (list-variables p2))
           (lv (sort (set-union lv1 lv2) compare-variables))
           (cp1 (contents (poly->canon p1 lv)))
           (cp2 (contents (poly->canon p2 lv))))
      (make-poly (car lv)
                 (f (coerce final-type (term-list cp1))
                    (coerce final-type (term-list cp2))))))
  (define (add-poly p1 p2)
    (op-poly add-terms p1 p2
             "Polys not in same var -- ADD-POLY"))
  (define (mul-poly p1 p2)
    (op-poly mul-terms p1 p2
             "Polys not in same var -- MUL-POLY"))
  <div-poly>
  (define (negate-poly p)
    (make-poly (variable p) (negate-terms (term-list p))))

  (define (zero-order-coeff tl)
    (cond ((null? tl) 0)
          ((=zero? (order (first-term tl))))
;           (order (first-term tl)))
          (else
           (zero-order-coeff (rest-terms tl)))))
]

@subsection{Term list representations}

Finally, the actual code and tests for the the dense and sparse term
list representations.

@chunk[<dense-term-representation>
(define (install-dense-package)
  (define (tag term-list) (attach-tag 'dense term-list))
  (define (first-term term-list)
    (make-term (order-dense term-list)
               (car term-list)))
  (define (rest-terms term-list)
    (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (the-empty-termlist term-list) '())

  (define (order-dense term-list)
    (- (length term-list) 1))
  (define (coeff-dense term-list)
    (car term-list))

  (define (strip-leading-zeros term-list)
    (cond ((null? term-list) '())
          ((=zero? (coeff-dense term-list))
           (strip-leading-zeros (rest-terms term-list)))
          (else
           term-list)))

  (define (adjoin-term term term-list)
    (define (adjoin term term-list)
      (cond ((and (empty-termlist? term-list)
                  (null? term))
             (the-empty-termlist term-list))
            ((null? term) term-list)
            ((empty-termlist? term-list)
             (if (=zero? (coeff term))
                 (the-empty-termlist term-list)
                 (expand-term-to-list term)))
            (else
             (let ((o1 (order term))
                   (o2 (order-dense term-list))
                   (c1 (coeff term))
                   (c2 (coeff (first-term term-list))))
               (cond ((< o1 o2)
                      (cons c2 (adjoin term (rest-terms term-list))))
                     ((> o1 o2)
                      (cons c1 (adjoin (make-term (- o1 1) 0)
                                       term-list)))
                     (else
                      (map add (expand-term-to-list term)
                           term-list)))))))
    (strip-leading-zeros (adjoin term term-list)))
  (define (expand-term-to-list term)
    (if (= (order term) 0)
        (list (coeff term))
        (cons (coeff term)
              (expand-term-to-list (make-term (- (order term) 1) 0)))))

  (define (all-zero-coeff? term-list)
    (=zero? (foldl add 0 term-list)))
  (define (zero-order-list? L)
    (cond ((empty-termlist? L) #t)
          ((= 1 (length L)) #t)
          ((=zero? (coeff (first-term L)))
           (zero-order-list? (rest-terms L)))
          (else #f)))

  (put 'make-termlist   '(dense) (lambda (tl) (tag (make-from-coeffs tl))))
  (put 'first-term      '(dense) (lambda (tl) (first-term tl)))
  (put 'rest-terms      '(dense) (lambda (tl) (tag (rest-terms tl))))
  (put 'empty-termlist? '(dense) (lambda (tl) (empty-termlist? tl)))
  (put 'the-empty-termlist '(dense) (lambda (tl) (tag (the-empty-termlist tl))))
  (put 'apply-adjoin-term '(dense) (lambda (tl)
                                     (lambda (t)
                                       (tag (adjoin-term t tl)))))
  (put 'zero-coeff-termlist? '(dense) (lambda (tl)
                                        (all-zero-coeff? tl)))
  (put 'zero-order-termlist? '(dense) (lambda (tl)
                                        (zero-order-list? tl)))
  (put 'canonical-form  '(dense) (lambda (tl) (tag (strip-leading-zeros tl))))

  <dense-conversion>
  <tower-level-dense>

  (define tests
    (test-suite
     "tests for dense representation for polynomials"
     (check-equal? '(term 2 2) (first-term '(2 1 0)))
     (check-equal? '(1 0) (rest-terms '(2 1 0)))
     (check-equal? '(3 1) (adjoin-term '(term 1 2) '(1 1)))
     (check-equal? '(1 0) (adjoin-term '(term 1 1) '()))
     (check-equal? '(2 1 1) (adjoin-term '(term 2 2) '(1 1)))
     (check-equal? '() (adjoin-term '(term 2 0) '()))
     (check-equal? '(3 0) (adjoin-term '(term 1 1) '(0 0 2 0)))
     (check-equal? '(1 0) (adjoin-term '(term 3 -3) '(3 0 1 0)))
     (check-equal? '(5 0 0 0 1 0) (adjoin-term '(term 3 -3) '(5 0 3 0 1 0)))
     (check-equal? '(1 0) (make-from-coeffs '(0 0 1 0)))
     (check-equal? '(dense 5 3 1) (add-terms '(dense 3 2 1) '(dense 2 1 0)))
     (check-equal? '(dense 3 4 2) (add-terms '(dense 3 2 1) '(dense 2 1)))
     (check-equal? '(dense 4 2) (mul-terms '(dense 2) '(dense 2 1)))
     (check-equal? '(dense 1 2 1) (mul-terms '(dense 1 1) '(dense 1 1)))
     (check-equal? '(dense -2 -1 0) (negate-terms '(dense 2 1 0)))
     (check-true (all-zero-coeff? '(0)))
     (check-true (all-zero-coeff? '(0 0)))
     (check-true (all-zero-coeff? (list 0 (make-rational 0 1))))
     (check-true (all-zero-coeff? (list (make-complex-from-real-imag 0 0)
                                        (make-rational 0 1))))
     (check-equal? '(2 0 0) (expand-term-to-list '(term 2 2)))
     (check-equal? '(2 0 1) (make-from-terms '((term 2 2) (term 0 1))))
     (check-equal? '((term 2 2) (term 1 0) (term 0 1)) (to-terms '(2 0 1)))
     ))
  (run-tests tests)
  )
]

@chunk[<sparse-term-representation>
(define (install-sparse-package)
  (define (tag p) (attach-tag 'sparse p))
  (define (adjoin-term term term-list)
    (if (null? term-list)
        (list (contents term))
        (let* ((t1 (first-term term-list))
               (rest (rest-terms term-list))
               (o1 (order t1))
               (ot (order term)))
          (cond ((=zero? (coeff term))
                 term-list)
                ((= ot o1)
                 (cons (add-term-same-order term t1) rest))
                ((> ot o1)
                 (cons (contents term) term-list))
                (else
                 (cons (contents t1)
                       (adjoin-term term rest)))))))

  (define (add-term-same-order t1 t2)
    (make-term-sparse (order t1) (add (coeff t1) (coeff t2))))

  (define (first-term term-list) (apply make-term (car term-list)))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (the-empty-termlist term-list) '())
  (define (zero-coeff-termlist? term-list)
    (if (null? term-list)
        #t
        (andmap =zero? (map coeff-sparse term-list))))
  (define (zero-order-termlist? term-list)
    (and (= 1 (length term-list))
         (=zero? (order (first-term term-list)))))

  (define (make-term-sparse order coeff)
    (list order coeff))
  (define (order-sparse t)
    (car t))
  (define (coeff-sparse t)
    (cadr t))

  (define (strip-zeros term-list)
    (cond ((null? term-list) '())
          ((=zero? (coeff-sparse (car term-list)))
           (strip-zeros (cdr term-list)))
          (else
           (cons (car term-list) (strip-zeros (cdr term-list))))))
  (define (canonical-form term-list)
    (make-from-terms (to-terms (strip-zeros term-list))))

  (put 'canonical-form  '(sparse) (lambda (tl) (tag (canonical-form tl))))
  (put 'make-termlist   '(sparse) (lambda (tl) (tag (canonical-form tl))))
  (put 'first-term      '(sparse) (lambda (tl) (first-term tl)))
  (put 'rest-terms      '(sparse) (lambda (tl) (tag (rest-terms tl))))
  (put 'empty-termlist? '(sparse) (lambda (tl) (empty-termlist? tl)))
  (put 'the-empty-termlist '(sparse) (lambda (tl) (tag (the-empty-termlist tl))))
  (put 'apply-adjoin-term '(sparse) (lambda (tl)
                                      (lambda (t)
                                        (tag (adjoin-term t tl)))))
  (put 'zero-coeff-termlist? '(sparse) (lambda (tl)
                                        (zero-coeff-termlist? tl)))
  (put 'zero-order-termlist? '(sparse) (lambda (tl)
                                         (zero-order-termlist? tl)))
  (put 'equ? '(sparse sparse) (lambda (tl1 tl2)
                                (and (equal? (map order-sparse tl1)
                                             (map order-sparse tl2))
                                     (andmap equ? (map coeff-sparse tl1)
                                             (map coeff-sparse tl2)))))
  <sparse-conversion>
  <tower-level-sparse>

  (define tests
    (test-suite
     "tests for sparse representation for polynomials"
     (check-equal? '(term 2 2) (first-term '((2 2) (1 1) (0 0))))
     (check-equal? '((1 1) (0 0)) (rest-terms '((2 2) (1 1) (0 0))))
     (check-equal? '((1 3) (0 1)) (adjoin-term (make-term 1 2) '((1 1) (0 1))))
     (check-equal? '((1 1)) (adjoin-term (make-term 1 1) '()))
     (check-equal? '((2 2) (1 1) (0 1)) (adjoin-term (make-term 2 2) '((1 1) (0 1))))
     (check-equal? '((term 2 2) (term 1 1) (term 0 1)) (to-terms '((2 2) (1 1) (0 1))))
     (check-equal? '(sparse (2 5) (1 3) (0 1)) (add-terms '(sparse (2 3) (1 2) (0 1))
                                                   '(sparse (2 2) (1 1) (0 0))))
     (check-equal? '(sparse (2 3) (1 4) (0 2)) (add-terms '(sparse (2 3) (1 2) (0 1))
                                                          '(sparse (1 2) (0 1))))
     (check-equal? '(sparse (1 4) (0 2)) (mul-terms '(sparse (0 2)) '(sparse (1 2) (0 1))))
     (check-equal? '(sparse (2 1) (1 2) (0 1)) (mul-terms '(sparse (1 1) (0 1))
                                                          '(sparse (1 1) (0 1))))
     (check-equal? '(sparse (2 -2) (1 -1) (0 0)) (negate-terms '(sparse (2 2) (1 1) (0 0))))
     (check-true (zero-coeff-termlist? '((0 0))))
     (check-true (zero-coeff-termlist? '((100 0) (10 0) (1 0))))
     (check-true (equ? '(sparse (3 1) (2 1) (0 1)) '(sparse (3 1) (2 1) (0 1))))
     (check-false (equ? '(sparse (3 1) (2 1) (0 1)) '(sparse (2 1) (0 1))))
     (check-equal? '((1 1)) (adjoin-term '(term 0 0) '((1 1))))
     (check-equal? '(sparse) (add-terms '(sparse (1 1)) '(sparse (1 -1))))
     (check-equal? '((term 2 1) (term 1 1) (term 0 1)) (to-terms '((2 1) (1 1) (0 1))))
     ))
  (run-tests tests)
  )
]

Now the complex to polynomial conversion function has to be changed as
well. This suggests that we could create generic make-term-list and
make-term functions, but I hold off on that for now.

@chunk[<complex-poly>
(define (complex->poly z)
  (make-polynomial-dense ANY-VARIABLE (list (tag z))))
(put 'raise '(complex) (lambda (x) (complex->poly x)))
]

@subsection{Supporting multiple coercion hierarchies}

I wanted to use the existing coercion facilities to coerce between
dense and sparse term lists. But term lists don't fit anywhere in the
type tower that relates, say, an integer and a complex number. We need
a way to represent multiple independent type hierarchies.

The form of the new coercion functions is:

@chunk[<type-tower-framework>
<make-type-tower-level>
<main-type-tower>
<tower-constants>
]

The following factory function creates a function that reports a
type's level in a type tower, where the tower is a list of type
tags (highest type last).

@chunk[<make-type-tower-level>
(define (make-type-tower-level tower)
  (lambda (type)
    (let ((max-level (length tower))
          (type-place (memq type tower)))
      (if type-place
          (- max-level (length type-place))
          #f))))
]

We can use this function to reimplement the main type tower:

@chunk[<main-type-tower>
<type-tower-with-ratpoly>
(define type-tower-main-level
  (make-type-tower-level type-tower-main))
]

We can then reimplement the type constants using the created function
@racket[type-tower-main-level]. @racket[LEVEL-LOWEST] is the lowest
level it is possible to report. @racket[ANY-VARIABLE] is the "unbound"
signfier for polynomials that don't care what variable they are bound
to.

@chunk[<tower-constants>
(define LEVEL-LOWEST 0)
(define ANY-VARIABLE   'ANY-VARIABLE)
(define LEVEL-INTEGER  (type-tower-main-level 'integer))
(define LEVEL-RATIONAL (type-tower-main-level 'rational))
(define LEVEL-REAL     (type-tower-main-level 'real))
(define LEVEL-COMPLEX  (type-tower-main-level 'complex))
(define LEVEL-POLY     (type-tower-main-level 'polynomial))
<tower-ratpoly>
]

So for we've just gotten back where we started with the behavior of
the main type tower--we've just changed the backing
implementation. But now things get interesting and the change we've
made shows its use.

We can also define a *totally separate* type tower for term lists. It
will be hidden within the scope of the polynomial package, but still
be able to use the generic application and coercion frameworks. So we
put the following code inside the polynomial package:

@chunk[<termlist-type-tower>
(define type-tower-termlist '(dense sparse))
(define type-tower-termlist-level
  (make-type-tower-level type-tower-termlist))
]

and then install this distinct tower level reporter into the two term
list packages:

@chunk[<tower-level-dense>
(put 'tower-level '(dense) (lambda (x) (type-tower-termlist-level 'dense)))
]

@chunk[<tower-level-sparse>
(put 'tower-level '(sparse) (lambda (x) (type-tower-termlist-level 'sparse)))
]

Note that I put dense lists below sparse lists, which is somewhat
arbitrary, but reflects the fact that sparse lists can represent very
large lists more efficiently.

@subsection{Bug-fixing generic application}

I also had to fix a bug.

A latent bug in the generic application framework has been causing
infinite looping when @racket[apply-generic] is given arguments it
doesn't expect. Specifically, if an argument didn't have a
@racket[tower-level] implementation, @racket[apply-generic] would keep
looking forever. This is because we were using apply-generic to
dispatch on @racket[tower-level] *within* the apply-generic code
itself. Big no no.

The problem happened most commonly in cases when apply-generic was
given arguments that weren't properly formed.

The fix is to bypass apply-generic and to use the new internal
function @racket[tower-check] to get the appropriate
@racket[tower-level] function directly for each argument, if it
exists. Then, we change @racket[raise-lowest] to use this new function
rather than mapping on tower-level itself. Mapping over tower-level
looked elegant, but there be dragons in mapping over generic
functions!

@chunk[<raise-lowest>
(define (raise-lowest args)
  (let* ((tower-levels (filter number? (map tower-check args))))
    (if (> (length tower-levels) 0)
        (let ((min-level (apply min tower-levels)))
          (map (lambda (x)
                 (if (eq? min-level (tower-check x))
                     (try-apply 'raise x x)
                     x))
               args))
        (error "No raise-able arguments -- RAISE-LOWEST" args))))

(define (tower-check x)
  (try-apply 'tower-level x #f))

(define (try-apply func arg alt)
  (let ((proc (get func (list (type-tag arg)))))
    (if proc
        (proc (contents arg))
        alt)))
]

@subsection{Tests}

@chunk[<tests-polynomial-dense>
(test-suite
 "tests for dense polynomial term list"
 (check-equal? (make-polynomial-dense 'x '(3 1))
               (make-polynomial-dense 'x '(3 1)))

 (check-equal? 1
               (add (make-polynomial-dense 'x '(1))
                    (make-polynomial-dense 'x '(0)))
               "zero poly addition is identity")

 (check-equal? (make-polynomial-dense 'x '(2 0 0 1 3))
               (add (make-polynomial-dense 'x '(1 0 0 0 1))
                    (make-polynomial-dense 'x '(1 0 0 1 2))))

 (check-equal? (make-polynomial-dense 'x '(1 2 1))
               (mul (make-polynomial-dense 'x '(1 1))
                    (make-polynomial-dense 'x '(1 1))))

 (check-equal? (make-polynomial-dense 'x '(1 2 1))
               (square (make-polynomial-dense 'x '(1 1))))

 (check-equal? (make-polynomial-dense 'x '(1 0 2))
               (add (make-polynomial-dense 'x '(1 0 0)) 2))

 (let ((a (make-polynomial-dense 'x '(1 0 0)))
       (b (make-polynomial-sparse 'x '((2 1) (0 2)))))
   (check-equal? (make-polynomial-dense 'x '(2 0 2))
                 (add a b))
   (check-equal? (make-polynomial-sparse 'x '((2 2) (0 2)))
                 (add b a)))

 (let* ((a (make-polynomial-dense 'x (list 2
                                     (make-polynomial-dense 'y '(1 1))
                                     5)))
        (b (make-polynomial-dense 'x (list 0
                                     2
                                     1)))
        (c (make-polynomial-dense 'x (list 2
                                     (make-polynomial-dense 'y '(1 3))
                                     6))))
   (check-true (equ? c (add a b)))
   (check-true (equ? c (add b a))))

 (let* ((a (make-polynomial-dense 'x '( 4  0 2  0 1)))
        (b (make-polynomial-dense 'x '( 5  4 0  1 1)))
        (c (make-polynomial-dense 'x '(-1 -4 2 -1 0))))
   (check-equal? c (sub a b))
   (check-equal? (make-polynomial-dense 'x '(4 0 2 0 0))
                (sub a 1)))

 (check-equal? (make-polynomial-dense 'x (list
                                    (make-polynomial-dense 'y '(-1 -1))
                                    (make-rational -1 2)
                                    -1))
               (negate
                (make-polynomial-dense 'x
                                 (list
                                  (make-polynomial-dense 'y '(1 1))
                                  (make-rational 1 2)
                                  1))))

 (check-equal? (make-polynomial-sparse 'y '((1 1)))
               (raise (make-polynomial-dense 'y '(1 0))))
 (check-equal? 3 (project (make-polynomial-dense 'x '(3))))
 (check-equal? (make-rational 2 3)
               (project (make-polynomial-dense 'x
                                               (list (make-rational 2 3)))))

 (check-true (=zero? (make-polynomial-dense 'x '(0))))
 (check-true (=zero? (make-polynomial-dense 'x '(0 0))))
 (check-false (=zero? (make-polynomial-dense 'x '(1))))
 (check-false (=zero? (make-polynomial-dense 'x '(1 0))))
 (check-equal? LEVEL-POLY (tower-level (make-polynomial-dense 'x '(1 0))))
 )
]

@chunk[<tests-polynomial-sparse>
(test-suite
 "tests for sparse polynomial term list"
 (check-equal? (make-polynomial-sparse 'x '((1 3) (0 1)))
               (make-polynomial-sparse 'x '((0 1) (1 2) (1 1))))
 (check-equal? (make-polynomial-sparse 'x '((4 2) (1 1) (0 3)))
               (add (make-polynomial-sparse 'x '((4 1) (0 1)))
                    (make-polynomial-sparse 'x '((4 1) (1 1) (0 2)))))
 (check-equal? (make-polynomial-sparse 'x '((2 1) (1 2) (0 1)))
               (mul (make-polynomial-sparse 'x '((1 1) (0 1)))
                    (make-polynomial-sparse 'x '((1 1) (0 1)))))
 (check-equal? (make-polynomial-sparse 'x '((2 1) (0 2)))
               (add (make-polynomial-sparse 'x '((2 1))) 2))
 (let* ((a (make-polynomial-sparse 'y (list '(1 1) (list 1 0) (list 0 1))))
        (b (make-polynomial-sparse 'x (list '(2 1) (list 1 a) '(0 5))))
        (c (make-polynomial-sparse 'x (list '(2 1) (list 1 2) '(0 1))))
        (d (make-polynomial-sparse 'x (list '(2 2)
                                     (list 1 (make-polynomial-sparse 'y
                                                              '((1 1) (0 3))))
                                     '(0 6)))))
   (check-equal? d (add b c)))

 (let* ((a (make-polynomial-sparse 'x '((4 4) (2 2) (0 1))))
        (b (make-polynomial-sparse 'x '((4 5) (3 4) (1 1) (0 1))))
        (c (make-polynomial-sparse 'x '((4 -1) (3 -4) (2 2) (1 -1)))))
   (check-equal? c (sub a b))
   (check-equal? (make-polynomial-sparse 'x '((4 4) (2 2)))
                 (sub a 1)))

 (check-equal? (make-polynomial-sparse 'x '((2 1) (1 2) (0 1)))
               (square (make-polynomial-sparse 'x '((1 1) (0 1)))))
 (check-equal? (make-polynomial-sparse 'x (list (list 2 (make-polynomial-sparse
                                                  'y '((1 -1) (0 -1))))
                                         (list 1 (make-rational -1 2))
                                         (list 0 -1)))
               (negate
                (make-polynomial-sparse 'x
                                 (list (list 2 (make-polynomial-sparse
                                                'y '((1 1) (0 1))))
                                       (list 1 (make-rational 1 2))
                                       (list 0 1)))))

 (check-equal? (make-polynomial-sparse 'y '((1 1)))
               (raise (make-polynomial-sparse 'y '((1 1)))))
 (check-equal? 3 (project (make-polynomial-sparse 'x '((0 3)))))
 (check-equal? (make-rational 2 3)
               (project (make-polynomial-sparse 'x
                                         (list (list 0 (make-rational 2 3))))))


 (check-true (=zero? (make-polynomial-sparse 'x '((0 0)))))
 (check-true (=zero? (make-polynomial-sparse 'x '((1 0)))))
 (check-false (=zero? (make-polynomial-sparse 'x '((0 1)))))
 (check-false (=zero? (make-polynomial-sparse 'x '((1 1)))))
 (check-equal? LEVEL-POLY (tower-level (make-polynomial-sparse 'x '((1 1)))))

 (check-equal? (list (make-polynomial-sparse 'x '((3 1) (1 1)))
                     (make-polynomial-sparse 'x '((1 1) (0 -1))))
               (div (make-polynomial-sparse 'x '((5 1) (0 -1)))
                    (make-polynomial-sparse 'x '((2 1) (0 -1)))))
 (check-equal? (list (make-polynomial-sparse 'x '((5 1/2) (0 -1/2)))
                     (make-polynomial-sparse 'x '()))
               (div (make-polynomial-sparse 'x '((5 1) (0 -1))) 2))
 )
]

@subsection{Polynomial package}

@chunk[<polynomial-package>
(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))

  ;; from section 2.3.2
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  <poly-expand-to-terms>

  <generic-termlist-install>
  <polynomial-operations>

  (define (any-variable? p)
    (eq? (variable p) 'ANY-VARIABLE))
  (define (constant? p)
    (zero-order-termlist? (term-list p)))
  (define (operate? p1 p2) #t)

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  <install-div-poly>
  <install-gcd-poly>

  (put '=zero? '(polynomial)
       (lambda (x) (zero-coeff-termlist? (term-list x))))

  (put 'tower-level '(polynomial) (lambda (x) LEVEL-POLY))

  (put 'project '(polynomial)
       (lambda (x)
         (if (empty-termlist? (term-list x))
             (make-scheme-number 0)
             (coeff (first-term (term-list x))))))
  (put 'raise '(polynomial)
       (lambda (p)
         (tag (make-poly (variable p) (raise (term-list p))))))

  (put 'equ? '(polynomial polynomial)
       (lambda (p1 p2)
          (and (or (same-variable? (variable p1) (variable p2))
                   (and (constant? p1) (constant? p2)))
               (equ? (term-list p1) (term-list p2)))))
  (put 'negate '(polynomial)
       (lambda (x) (tag (negate-poly x))))
  (put 'sub '(polynomial polynomial)
     (lambda (p1 p2) (tag (add-poly p1 (negate-poly p2)))))
  (put 'make '(polynomial)
       (lambda (var terms) (tag (make-poly var terms))))

  (run-tests (test-suite
              "tests for internal polynomial procedures"
              <tests-polynomial-indeterminates>
              (check-true (compare-variables 'ANY-VARIABLE #f))
              (check-false (compare-variables #f 'ANY-VARIABLE))
              (check-true (compare-variables 'x #f))
              (check-true (compare-variables 'x 'xa))
              (check-true (compare-variables 'xa 'xb))
              (check-true (compare-variables 'x #f))
              (check-true (compare-variables 'x 'ANY-VARIABLE))
              (check-false (compare-variables #f 'x))
              (check-false (compare-variables 'ANY-VARIABLE 'x))
              (check-false (compare-variables 'x 'x))
              (check-true (constant?
                           (make-poly 'x '(sparse (0 0)))))
              (check-false (constant?
                            (make-poly 'x '(sparse (1 1)))))
              (check-true (constant?
                           (make-poly 'x '(dense)))
                          "empty dense term-list is a constant polynomial")
              (check-true (constant?
                           (make-poly 'x '(dense 1)))
                          "singular dense term-list is a constant")
              (check-true (constant?
                           (make-poly 'x '(dense 4)))
                          "singular dense term-list is a constant")
              (check-true (constant?
                            (make-poly 'x '(dense 0 2)))
                           "dense term-list with leading zeros is a constant")
              (check-false (constant?
                            (make-poly 'x '(dense 1 0 2)))
                           "dense term-list is not constant")
              (check-equal? '((3 (x 3)) (2 (x 2)) (1 (x 1)) (1 (x 0)))
                            (poly-expand-to-terms (make-poly 'x '(dense 3 2 1 1))))
              (check-equal? '((2 (x 4) (y 1)) (1 (x 4) (y 0)) (1 (x 3)))
                            (poly-expand-to-terms (make-poly
                                                   'x
                                                   (list 'sparse
                                                         (list 4 (make-polynomial-dense 'y '(2 1)))
                                                         '(3 1)))))
              (check-equal? '(x)
                            (list-variables
                             (make-poly 'x '(dense 2 1))))
              (let* ((zp (make-polynomial-dense 'z '(1 0)))
                     (yp (make-polynomial-dense 'y (list 5 4 zp 2 1)))
                     (xp (make-polynomial-dense 'x (list yp 1 0 0 0)))
                     (rp (make-polynomial-dense 'z (list xp 1 yp))))
                (check-equal? '(x y z)
                              (list-variables (contents xp)))
                (check-equal? '(x y z)
                              (list-variables (contents rp))))

              (check-true (compare-expand-terms '(x 2) '(x 1)))
              ))
  )
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
(require racket/trace)
       <helper-fns>
       <arithmetic-api>
       <generic-dispatch-framework>
       <numbers-packages>
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
  (if (equal? '(scheme-number scheme-number)
              (map type-tag (list n d)))
      ;; reduce long integer rationals
      (let ((n-len (int-len n))
            (d-len (int-len d)))
        (if (and (>= n-len 4) (>= d-len 4))
            (make-scheme-number (/ n d))
            ((get 'make 'rational) n d)))
      ((get 'make 'rational) n d)))
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))
<make-polynomial>
]

@chunk[<generic-operations>
(define (add x y)  (apply-generic 'add x y))
(define (sub x y)  (apply-generic 'sub x y))
(define (mul x y)  (apply-generic 'mul x y))
(define (div x y)  (apply-generic 'div x y))
(define (sine n) (apply-generic 'sine n))
(define (cosine n) (apply-generic 'cosine n))
(define (arctan x y) (apply-generic 'arctan x y))
(define (sqroot n) (apply-generic 'sqroot n))
(define (negate n) (apply-generic 'negate n))
(define (equ? a b) (apply-generic 'equ? a b))
(define (=zero? a) (apply-generic '=zero? a))
(define (raise n)  (apply-generic 'raise n))
(define (tower-level n) (apply-generic 'tower-level n))
(define (project n) (apply-generic 'project n))
(define (greatest-common-divisor x y) (apply-generic 'greatest-common-divisor x y))
<drop-with-ratpoly-check>
(define DROPPABLE '(add sub mul div sine cosine arctan sqroot negate))
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
  <raise-lowest>

  (let ((type-tags (filter symbol? (map type-tag args))))
    (let ((proc (get op type-tags)))
      (if proc
          (let ((res (apply proc (map contents args))))
            (if (memq op DROPPABLE)
                (drop res)
                res))
          (let* ((raised-args (raise-lowest args))
                 (raised-type-tags (map type-tag raised-args)))
            (if (not (andmap equal? raised-type-tags type-tags))
                (apply apply-generic (cons op raised-args))
                (no-method op type-tags)))))))
]

@chunk[<tagged-data-accessors>
(define (attach-tag type-tag datum)
  (cons type-tag datum))

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
<type-tower-framework>
<scheme-number-package>
<rational-number-package>
<complex-number-package>
<polynomial-package>
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
  (put 'sine '(scheme-number)
       (lambda (x) (tag (sin x))))
  (put 'cosine '(scheme-number)
       (lambda (x) (tag (cos x))))
  (put 'arctan '(scheme-number scheme-number)
       (lambda (x y) (tag (atan x y))))
  (put 'sqroot '(scheme-number)
       (lambda (x) (tag (sqrt x))))
  (put 'negate '(scheme-number)
       (lambda (x) (tag (- x))))

  <install-gcd-number>

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
  <internal-make-rat>
  <internal-rational-operations>

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
  (put 'negate '(rational)
     (lambda (x)
       (tag (make-rat (negate (numer x))
                      (denom x)))))

  ;; coercion functions
  (define (rational->real n)
    (make-scheme-number
     (exact->inexact (/ (numer n) (denom n)))))

  <rational-coercion-with-ratpoly>
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

  <complex-poly>

  (define (negate-complex z)
    (make-from-real-imag (negate (real-part z))
                         (negate (imag-part z))))
  (put 'negate '(complex)
       (lambda (z) (tag (negate-complex z))))

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
      (sqroot (add (square (real-part z))
                   (square (imag-part z)))))
    (define (angle z)
      (arctan (imag-part z) (real-part z)))
    (define (make-from-mag-ang r a)
      (cons (mul r (cosine a)) (mul r (sine a))))

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
    (define (real-part z)
      (mul (magnitude z) (cosine (angle z))))
    (define (imag-part z)
      (mul (magnitude z) (sine (angle z))))
    (define (make-from-real-imag x y)
      (cons (sqroot (add (square x) (square y)))
            (arctan y x)))

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
(install-polynomial-package)
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
(define (square n) (mul n n))
(define (exact-if-possible n)
  (if (integer? n)
      (inexact->exact n)
      n))
(define (int-len n)
  (define (iter x acc)
    (if (= x 0) acc
        (iter (truncate (/ x 10))
              (+ 1 acc))))
  (iter n 0))
(define (unique a)
  (define (iter l acc)
    (if (null? l) acc
        (if (member (car l) acc)
            (iter (cdr l) acc)
            (iter (cdr l) (cons (car l) acc)))))
  (iter a '()))

(define (set-union a b)
  (cond ((null? a) b)
        ((null? b) a)
        ((member (car b) a)
         (set-union a
                    (cdr b)))
        (else
         (set-union (cons (car b) a)
                    (cdr b)))))
(define (set-intersect a b)
  (cond ((null? a) '())
        ((null? b) '())
        ((member (car b) a)
         (cons (car b)
               (set-intersect a (cdr b))))
        (else
         (set-intersect a (cdr b)))))
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
   <tests-polynomial-dense>
   <tests-polynomial-sparse>
   <tests-equ?-op>
   <tests-zero?-op>
   <tests-coercions>
   <tests-raise>
   <tests-tower-level>
   <tests-project>
   <tests-drop>
   <tests-internal-complex>
   <tests-trig>
   <tests-negate>
;   <tests-rational-functions>
   <tests-gcd>
   ))
(run-tests all-tests)
]

@chunk[<tests-negate>
(test-suite
 "tests for negate generic operation"
 (check-equal? -1 (negate (make-scheme-number 1)))
 (check-equal? (make-rational -1 2) (negate (make-rational 1 2)))
 (check-equal? (make-complex-from-real-imag -1 -1)
               (negate
                (make-complex-from-real-imag 1 1)))
 (check-equal? (make-complex-from-real-imag
                (make-rational -1 2) -3)
               (negate
                (make-complex-from-real-imag
                 (make-rational 1 2) 3)))
 )
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
 (check-equal? 0 (tower-level 1))
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
