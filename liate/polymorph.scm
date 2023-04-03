(define-module (liate polymorph)
  #:use-module (oop goops)
  #:use-module (liate utils hashmaps)
  #:use-module (liate utils lists)
  #:export (ref <hashmap>)
  #:duplicates (merge-generics))

(define <hashmap>
  (class-of (hashmap)))

(define-generic ref)

(define-method (ref thing idx failure)
  "A wrapper for (ref thing idx failure success) which gives success a default value"
  (ref thing idx failure identity))

(define-method (ref thing idx)
  "A wrapper for (ref thing idx failure success) which gives failure and success default values"
  (ref thing idx (λ () (error "Invalid ref" thing idx))))

(define-method (ref (hm <hashmap>) key failure success)
  (hashmap-ref hm key failure success))

(define* (exceptions->failure/success thunk failure success #:optional (unwind-type #t))
  (with-exception-handler
      (λ (exn)
        (failure))
    (λ () (success (thunk)))
    #:unwind-for-type unwind-type))

(define-method (ref (lst <list>) (idx <integer>) failure success)
  (exceptions->failure/success
   (λ () (list-ref lst idx))
   failure
   success
   'out-of-range))

(define-method (ref (vect <vector>) (idx <integer>) failure success)
  (with-exception-handler
      (λ (exn)
        (failure))
    (λ ()
      (success (vector-ref vect idx)))
    #:unwind-for-type 'out-of-range))

(define-method (ref (ra <array>) (idxen <list>) failure success)
  (with-exception-handler
      (λ (exn)
        (failure))
    (λ ()
      (success (apply array-ref ra idxen)))
    #:unwind-for-type 'misc-error))
