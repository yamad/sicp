#lang racket

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

(define (make-key op type)
  (list op type))
(define (put-generic op type item table)
  (let ((key (make-key op type)))
    (cons (cons key item) (prune-key key table))))
(define (get-generic op type table)
  (let ((match (assoc (make-key op type) table)))
    (if match
        (cdr match)
        #f)))

(provide prune-key)
(provide make-key)

; tests
(equal? (prune-key 'a '()) '())
(equal? (prune-key 'a '((a b) (c d))) '((c d)))
(equal? (prune-key 'c '((a b) (c d))) '((a b)))
(equal? (prune-key 'c '((a b) (c d) (e f))) '((a b) (e f)))
(equal? (prune-key 'b '((a b) (c d))) '((a b) (c d)))
(equal? (prune-key 'e '((a b) (c d))) '((a b) (c d)))

(equal? (put-generic 'a 'b 'c '()) '(((a b) . c)))
(equal? (put-generic 'a 'b 'd '()) '(((a b) . d)))
(equal? (put-generic 'a 'b 'c '( ((d e) . f)) )
        '( ((a b) . c)
           ((d e) . f) ))
(equal? (put-generic 'a 'b 'd '( ((a b) . c)
                                 ((d e) . f) ))
        '( ((a b) . d)
           ((d e) . f) ))

(equal? (get-generic 'a 'b '( ((a b) . c))) 'c)
(equal? (get-generic 'a 'b '( ((d e) . f)
                              ((a d) . g)
                              ((a b) . c)))
        'c)
