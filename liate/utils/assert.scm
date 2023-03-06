(define-module (liate utils assert)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 match)
  #:use-module (liate utils proc-tools)
  #:use-module (liate utils lists)
  #:export (assert assure))

;;; Assert macro
;;; Based on [[https://okmij.org/ftp/Scheme/util.html#assert]],
;;; but taking advantage of `syntax-case` and keywords
    

;;; Helper functions 

(define (create-assertion-failure mesg var-binding-alist)
  "Creates a compound exception representing a (@ (liate utils assert) assert) failure."
  (make-exception (make-assertion-failure)
                  (make-exception-with-message mesg)
                  (make-exception-with-irritants var-binding-alist)))

(define (on-failure mesg vars)
  "Creates syntax for creating and throwing an assertion failure"
  #`(raise-exception
     (create-assertion-failure
      #,mesg
      `#,(map (λ (id)
                #`(#,id ,#,id))
              vars))))

(define (actual-syntax-object syntax-list)
  "Returns a syntax object from SYNTAX-LIST, which is either a syntax object or
a tree of syntax objects.

Necessary because everything but `datum->syntax` will happily work with either."
  (if (list? syntax-list)
      (actual-syntax-object (car syntax-list))
      syntax-list))

(define (vars-in expr)
  "From [[https://okmij.org/ftp/Scheme/assert.txt]]"
  (map (curry datum->syntax (actual-syntax-object expr))
       (let loop ((expr (syntax->datum expr)) (vars '()))
         (cond
          ((not (pair? expr)) vars)     ; not an application -- ignore
          ((memq (car expr) 
                 '(quote let let* letrec let*-values lambda cond quasiquote
                         case define do assert))
           vars)                        ; won't go there
          (else                         ; ignore the head of the application
           (let inner ((expr (cdr expr)) (vars vars))
             (cond 
              ((null? expr) vars)
              ((symbol? (car expr))
               (inner (cdr expr)
                      (if (memq (car expr) vars) vars (cons (car expr) vars))))
              (else
               (inner (cdr expr) (loop (car expr) vars))))))))))

(define (syntax-split syntax!list separator?)
  "Splits a syntax object of a list as list-split splits lists"
  (let ((splitted (list-split (syntax->datum syntax!list) separator?)))
    (map (curry map (curry datum->syntax (actual-syntax-object syntax!list)))
         splitted)))

(define-syntax assert
  (lambda (x)
    "Asserts the truth of a series of expressions.

If (and EXPR EXPRS ...) returns truthy, evaluates to the return value.
Otherwise, throws an &assertion-failure exception.

By default, the exception message includes the failed expressions, and
includes the names and values of the variables referenced in them as irritants.

If passed #:report as an argument, the exception message is contructed from
the expressions following it, typically a combination of string literals and
identifiers.

Based on okmij's assert macros, but ported to `syntax-case`. and guile"
    (syntax-case x ()
      ((_ expr)
       #`(or expr
             #,(on-failure (format #f "failed assertion: ~a" (syntax->datum #'expr))
                           (vars-in #'expr))))
      ((_ expr expr* ...)
       (match-let (((exprs . report-part)
                    (syntax-split (cons #'expr #'(expr* ...))
                                  (curry equal? #:report))))
         #`(or (and #,@exprs)
               #,(match report-part
                   (((mesg . vars))
                    (on-failure mesg vars))
                   (__
                    (on-failure (format #f "failed assertions:~{ ~a~}" (syntax->datum exprs))
                                (vars-in (cons #'and exprs)))))))))))

(define-syntax assure
  (lambda (x)
    (syntax-case x ()
      ((_ exp error-msg)
       #`(assert exp #:report error-msg . #,(vars-in #'exp))))))
