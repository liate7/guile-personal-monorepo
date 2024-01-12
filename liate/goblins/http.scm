(define-module (liate goblins http)
  #:use-module (goblins)
  #:use-module (goblins vat)
  #:use-module (goblins actor-lib common)
  #:use-module (goblins actor-lib methods)
  #:use-module (web client)
  #:use-module (liate goblins)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 textual-ports)
  #:export (^http-manager))

(define default-user-agent
  "(@ (liate goblins http) ^http-manager)")

(define (^http-manager _bcom)
  ;; (define cache (spawn ^http-cache))

  (define (request method url body decoder
                   extra-headers version keep-alive? user-agent force)
    ;; If the cache is fresh and (not force), return the cached value
    ;; If the cache is stale and (not force), send the request with if-modified-since
    ;;  set to either the last-modified date or the date of the cached value
    ;; If the cache is empty or FORCE is true, send the bare request

    ;; Always call DECODER on the output port, or a default
    ;; Cache the output unless have no-cache in cache-control
    #f)

  (methods
   ((get url #:key (extra-headers '()) decoder
         body version keep-alive? user-agent force)
    (let ((headers (cons* (cons 'user-agent (or user-agent default-user-agent))
                          extra-headers))
          (decoder (or decoder
                       (λ (_resp port)
                         (get-string-all port)))))
      (on (fibrous
           (call-with-values
               (λ ()
                 (http-request url
                               #:method 'GET
                               #:streaming? #t
                               #:headers headers
                               #:body body
                               #:version (or version '(1 . 1))
                               #:keep-alive? keep-alive?))
             list))
          (λ (lst)
            (apply decoder lst))
          #:promise? #t)))))

(define (^http-cache _bcom)
  (define method-url->varies (spawn ^ghash))
  (define varies->cached (spawn ^ghash))

  (methods
   ((ref method url decoder headers if-stale if-empty)
    (if-empty))
   ((cache method url response body)
    #f)))
