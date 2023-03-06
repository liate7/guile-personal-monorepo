(define-module (liate utils vectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-43)
  #:use-module (liate utils alists)
  #:use-module (liate utils macros)
  #:export (vector-mod vector-mod!))

(define (vector-mod vect i proc . rest)
  "Creates a copy of VECT where element I_n stores the result of calling PROC_n with
the current value at I_n.  All I's must be valid indices of VECT."
  (if (null? rest)
      (vector-map (lambda (index val)
                    (if (= index i)
                        (proc val)
                        val))
                  vect)
      (let ((index-functions
             (alist-cons i proc
                         (plist->alist rest))))
        (vector-map (lambda (index val)
                      (if-let (proc (assoc-ref index-functions index))
                              (proc val)
                              val))
                    vect))))

(define (vector-mod! vect i proc . rest)
  "Sets element I_n to the result of calling PROC_n with the current value at I_n,
for each I and PROC given.  All I's must be valid indices of VECT."
  (vector-set! vect i
               (proc (vector-ref vect i)))
  (if (null? rest) vect
      (apply vector-mod! vect rest)))
