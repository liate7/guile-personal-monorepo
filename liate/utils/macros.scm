(define-module (liate utils macros)
  #:use-module (liate utils proc-tools)
  #:export (if-let
            when-let
            repeat
            repeat-thunk
            update-place! zap!
            failure->))

;;; Combined binding-conditional macros

(define-syntax-rule (if-let (var form) then else)
  "Binds VAR to the result of evaluating FORM, then executes THEN if truthy or
ELSE if falsy.

Like the common `aif` macro, but hygenic."
  (let ((var form))
    (if var
        then
        else)))

(define-syntax-rule (when-let (var form) body ...)
  "Binds VAR to the result of evaluating FORM, then executes THEN if truthy.

A when equivalent to `if-let`"
  (let ((var form))
    (when var
      body ...)))

;;; Basic looping

(define (repeat-thunk n thunk)
  "Calls THUNK N times."
  (for-each (λ _ignore (thunk))
            (iota n)))

(define-syntax-rule (repeat n body ...)
  "Evaluates BODY N times."
  (repeat-thunk n (λ () body ...)))

;;; Update-in-place macros

(define-syntax-rule (update-place! place proc args ...)
  "Sets PLACE to the result of calling (apply proc place args).
A lispy generalization of C-family +=, -=, etc."
  (set! place (proc place args ...)))

(define-syntax zap!
  (lambda (x)
    "Sets PLACE to the result of evaluating EXPR with % bound to the value at PLACE.
Like CL zapf, as defined [[https://stevelosh.com/blog/2016/08/playing-with-syntax/]]"
    (syntax-case x ()
      ((_ place expr)
       #`(set! place
               (let ((#,(datum->syntax x '%) place))
                 expr))))))

(define-syntax failure->
  ;; A convenience macro for dealing with functions that take a failure continuation
  ;; Converts (failure-> (foo) (bar) (baz)) into (foo (lambda () (bar (lambda () (baz)))))
  (syntax-rules ()
    ((failure-> expr)
     expr)
    ((failure-> (expr ...) next next* ...)
     (expr ...
           (lambda ()
             (failure-> next next* ...))))))
