(define-module (liate utils comparators)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-128)

  #:re-export (comparator?
               comparator-hashable? comparator-ordered?
               make-comparator make-pair-comparator make-list-comparator make-vector-comparator
               make-eq-comparator make-eqv-comparator make-equal-comparator
               hash-bound
               comparator-type-test-predicate comparator-equality-predicate
               comparator-ordering-predicate comparator-hash-function comparator-test-type
               comparator-check-type comparator-hash
               =? <? >? <=? >=?
               comparator-if<=>)
  #:export (default-comparator
             comparator-hashx-assoc
             comparator-sized-hash-function))

(define default-comparator (make-equal-comparator))

(define (comparator-hashx-assoc comparator)
  "Creates an assoc-like function using COMPARATOR,
suitable for passing to hashx- family procedures."
  (lambda (key alist)
    (find (λ (cons) (=? comparator key (car cons))) alist)))

(define (comparator-sized-hash-function comparator)
  "Creates a function like COMPARATOR's hash, but
which takes an explicit size for the hash bound,
suitable for passing to hashx- family procedures.

Since Guile's srfi-128 implementation has a constant HASH-BOUND,
the size is ignored."
  (let ((hash (comparator-hash-function comparator)))
    (lambda (obj _size)
      (hash obj))))
