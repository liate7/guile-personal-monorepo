(define-module (liate utils json)
  #:use-module (srfi srfi-180)
  #:use-module ((rnrs io ports)
                #:select (eof-object))
  #:use-module (liate utils hashmaps)
  #:use-module (liate utils proc-tools)
  #:use-module (ice-9 match)

  #:export (json-read-hashmap))

(define (json-read-hashmap string-or-port)
  (define %root '(root))

  (define array-start (const '()))
  (define array-end reverse)
  
  (define (object-start _obj)
    (vector (hashmap)))
  (define (object-end acc)
	(match acc
      (#((? hashmap? hm)) hm)))
  
  (define (proc obj seed)
	(match seed
      ((? (curry eq? %root)) obj)
      (#((? hashmap? acc))
       (vector (string->symbol obj) acc))
      (#((? symbol? key) (? hashmap? acc))
       (vector (hashmap-set acc key obj)))
      ((? list?)
       (cons obj seed))))

  (define (internal port)
    (let ((ret (json-fold proc
                          array-start
                          array-end
                          object-start
                          object-end
                          %root
                          port)))
      (if (eq? ret %root)
          (eof-object)
		  ret)))
  (if (string? string-or-port)
      (call-with-input-string string-or-port internal)
      (internal string-or-port)))
