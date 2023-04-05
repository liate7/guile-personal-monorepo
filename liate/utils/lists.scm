(define-module (liate utils lists)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-171)
  #:use-module (ice-9 match)
  #:use-module (liate utils proc-tools)
  #:use-module (liate utils alists)
  #:use-module (liate utils math)
  #:export (flatten
            partition-by
            sum prod
            segment
            find-duplicates
            list-split
            tuples-in
            list-index-all
            inclusive-range
            group-by
            mapcat))

(define (flatten lst)
  "As the `tflatten' transducer, but just for lists"
  (list-transduce tflatten rcons lst))

(define (partition-by proc lst)
  "Partitions the list into runs where PROC returns the same value.
As the `tpartition' transducer, but just for lists"
  (list-transduce (tpartition proc) rcons lst))

(define (sum lst)
  (apply + lst))

(define (prod lst)
  (apply * lst))

(define (segment n lst)
  "Break up a list in to segments of N, based on `tsegment'"
  (list-transduce (tsegment n) rcons lst))

(define (find-duplicates lst)
  "Creates a list of all items in LST which appear more than once"
  (define duplicates-sum
    (match-lambda*
      ((item #(duplicates seen))
       (if (member item seen)
           (vector (lset-adjoin equal? duplicates item) seen)
           (vector duplicates (cons item seen))))))
  (vector-ref (fold duplicates-sum #(() ()) lst)
              0))

(define (list-split lst is-separator?)
  "Split the list LST into a list of sublists delimited by (lists of) elements
that satisfy IS-SEPARATOR?.

Unlike string-split, list-split coalesces sublists of separators, so multiple
separator elements next to each other will NOT result in an empty list in the
result."
  (define (splitter-sum val acc)
    (if (is-separator? val)
        (if (null? (car acc))
            acc
            (cons* '() (reverse (car acc)) (cdr acc)))
        (cons (cons val (car acc)) (cdr acc))))
  (reverse ((pair-juxt (compose reverse car) cdr)
            (fold splitter-sum '(()) lst))))

(define (tuples-in lst . lsts)
  "Creates a list of all the lists you can make by choosing a random element from
each of (cons LST LSTS) in order.

   (tuples-in '(1 2) '(3 4))
   ==>
   ((2 3) (2 4) (1 3) (1 4))"
  (define (tuples-sum item res)
    (append (map (curry cons item)
                 (apply tuples-in lsts))
            res))
  (if (null? lsts)
      (map list lst)
      (fold tuples-sum '() lst)))

(define (list-index-all pred lst . lsts)
  "Creates a list of /all/ the indices where applying PRED to the given lists
returns true.

Like list-index, but returns all the indices, not just the first one."
  (let ((first-idx (apply list-index pred lst lsts)))
    (if first-idx
        (cons first-idx
              (map (curry + 1 first-idx)
                   (apply list-index-all pred
                          (map (λ (l) (list-cdr-ref l (1+ first-idx)))
                               (cons lst
                                     lsts)))))
        '())))

(define* (inclusive-range from to #:optional (step 1))
  "Creates a list of items starting with FROM and ending with the first number >= TO,
where the distance between each item is STEP."
  (let ((n-between (ceiling/ (- to from) step)))
    (iota (1+ (abs n-between)) from (* (sign n-between) step))))

(define* (group-by proc lst)
  (define (group-sum val acc)
    (assoc-update acc (proc val) (curry cons val) (const (list val))))
  ((pair-juxt car (compose reverse cdr))
   (fold group-sum '() lst)))

(define (mapcat proc lst1 . lsts)
  (concatenate
   (apply map proc lst1 lsts)))
