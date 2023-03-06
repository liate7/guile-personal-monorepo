(define-module (liate utils alists)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (liate utils proc-tools)
  #:use-module (liate utils macros)
  #:export (assoc-set
            plist->alist alist->plist
            assoc-update assoc-update!
            alist-<=?))

(define (assoc-set alist key val)
  "Creates a copy of ALIST where KEY is associated with VAL and any previous
associations with KEY are removed"
  (let ((k-v (cons key val)))
    (let rec ((cur alist)
              (acc '())
              (found? #f))
      (cond ((and (null? cur) found?)
             (reverse acc))
            ((null? cur)
             (cons k-v (reverse acc)))

            ((and (equal? (caar cur) key) found?)
             (rec (cdr cur) acc #t))
            ((equal? (caar cur) key)
             (rec (cdr cur) (cons k-v acc) #t))

            (else
             (rec (cdr cur) (cons (car cur) acc) found?))))))

(define plist->alist
  (match-lambda
    ((key val . rest)
     (alist-cons key val
                 (if (< (length rest) 2)
                     '()
                     (plist->alist rest))))
    (() '())))

(define (alist->plist alist)
  (fold (match-lambda*
          (((key . val) acc)
           (cons* key val acc)))
        '()
        alist))

(define (key-error key)
  (λ () (error "No such key ~a" key)))

(define* (assoc-update alist key proc #:optional (default-thunk (key-error key)))
  "Creates a copy of ALIST where KEY is associated with (proc (assoc-ref alist key)).

If KEY is not in the alist, it is associated with the result of calling DEFAULT-THUNK."
  (assoc-set alist key
             (if-let (val (assoc-ref alist key))
                     (proc val)
                     (default-thunk))))

(define* (assoc-update! alist key proc #:optional (default-thunk (key-error key)))
  "Creates a (possibly new) copy of ALIST where KEY is associated with
(proc (assoc-ref alist key)).

If KEY is not in the alist, it is associated with the result of calling DEFAULT-THUNK.

Like `assoc-set!`, may or may not create a new alist or modify ALIST."
  (assoc-set! alist key
              (if-let (val (assoc-ref alist key))
                      (proc val)
                      (default-thunk))))

(define (alist-<=? gt lt)
  "Returns true if all keys in LT are in GT and if all shared keys are associated
with equal? values."
  (let rec ((lt lt))
    (cond ((null? lt) #t)
          ((equal? (assoc-ref gt (caar lt))
                   (cdar lt))
           (rec (cdr lt)))
          (else #f))))
