(define-module (liate utils hash-tables)
  #:use-module (liate utils macros)
  #:export (hash-mod! hash-table-keys))

(define (key-error key)
  (λ () (error "No such key ~a" key)))

(define unique (list 'empty))

(define* (hash-mod! table key proc #:optional (default-thunk (key-error key)))
  "Find the entry in TABLE associated with KEY, and store (proc (hash-ref table key))
there.  If KEY is not associated with any value in TABLE, uses the value of
calling DEFAULT-THUNK.
Uses `equal?` for equality testing"
  (hash-set! table key
             (let ((val (hash-ref table key unique)))
               (if (eq? val unique)
                   (default-thunk)
                   (proc (hash-ref table key))))))

(define (hash-table-keys table)
  "Creates a list of all keys currently contained in TABLE."
  (hash-fold (λ (key _val acc) (cons key acc))
             '()
             table))
