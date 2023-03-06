(define-module (liate utils proc-tools)
  #:use-module (ice-9 curried-definitions)
  #:export (curry
            proc-and proc-or
            fixed-point
            juxt pair-juxt
            arg
            memoized-lambda))

(define ((curry proc . original) . after)
  "Partially applies PROC to the given arguments;
i.e. returns a function that applies PROC to both the given arguments and 
the given new arguments"
  (apply proc (append original after)))

(define ((proc-and proc . procs) . args)
  "Creates a predicate that is satisfied if the given predicates are all
satisfied.

The resulting predicate properly short-circuits, like normal and."
  (and-map (λ (proc)
             (apply proc args))
           (cons proc procs)))

(define ((proc-or proc . procs) . args)
  "Creates a predicate that is satisfied if any of the given predicates are
satisfied.

The resulting predicate properly short-circuits, like normal or."
  (or-map (λ (proc)
            (apply proc args))
          (cons proc procs)))

(define* (fixed-point proc init #:key (equal equal?) (max-steps 100) (diverged-thunk (const #f)))
  "Calls PROC successively, first on INIT and then on the result of the previous call,
until either a fixed point has been found (the result is equal to the previous
result by EQUAL) or PROC has been called more than MAX-STEPS times.

If a fixed point is not found in less than MAX-STEPS steps, DIVERGED-THUNK
is called and its result returned.  If a fixed point is found, it is returned."
  (let rec ((prev init)
            (cur (proc init))
            (steps-remaining max-steps))
    (cond ((equal prev cur)
           cur)
          ((zero? steps-remaining)
           (diverged-thunk))
          (else
           (rec cur (proc cur) (1- steps-remaining))))))

(define ((juxt . procs) . vals)
  "Creates a procedure which returns a list of what PROCS returned when called
with the given values."
  (map (λ (proc) (apply proc vals)) procs))

(define ((pair-juxt a-proc d-proc) . vals)
  "Creates a procedure which returns a pair of what the given procedures returned
when called with the given values."
  (cons (apply a-proc vals)
        (apply d-proc vals)))

(define ((arg n) . args)
  "Creates a procedure which returns its N'th argument."
  (list-ref args n))

(define-syntax memoized-lambda
  ;; Creates a memoized anonymous procedure.
  (syntax-rules (memoized args val)
    ((_ arglst body ...)
     (let ((memoized (make-weak-key-hash-table)))
       (match-lambda*
         ((and args arglst)
          (if-let (val (hash-ref memoized args))
                  val
                  (let ((val (begin body ...)))
                    (hash-set! memoized args val)
                    val))))))))
