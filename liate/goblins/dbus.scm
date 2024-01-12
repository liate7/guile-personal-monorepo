(define-module (liate goblins dbus)
  #:use-module (goblins)
  #:use-module (goblins vat)
  #:use-module (goblins actor-lib common)
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins actor-lib facet)
  #:use-module (goblins ghash)
  #:use-module (d-bus protocol connections)
  #:use-module (d-bus protocol messages)
  #:use-module (d-bus protocol signatures)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module (fibers operations)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 match)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 curried-definitions)
  #:use-module ((sxml simple)
                #:select (xml->sxml
                          sxml->xml))
  #:use-module ((sxml match)
                #:select (sxml-match))
  #:use-module ((sxml xpath)
                #:select (sxpath))
  #:use-module (liate utils assert)

  #:export (spawn-bus))

(define (spawn-bus addr)
  (define bus-connection
    (d-bus-connect
     (concretize-addr addr)))

  (define serial->ret
    (spawn ^ghash))

  (define (^bus bcom)
    (methods
     ((send path dest interface method signature #:rest args)
      (let* ((promise resolve (spawn-promise-values))
             (serial (d-bus-conn-next-serial! bus-connection))
             (mesg (make-d-bus-message MESSAGE_TYPE_METHOD_CALL 0 serial '()
                                       `#(,(header-PATH path)
                                          ,(header-DESTINATION dest)
                                          ,(header-INTERFACE interface)
                                          ,(header-MEMBER method)
                                          ,@(if signature
                                                (list (header-SIGNATURE signature))
                                                '()))
                                       (if (null? args)
                                           #f
                                           args))))
        ($ serial->ret 'set serial resolve)
        (fibrous
         (d-bus-write-message bus-connection mesg)
         (d-bus-conn-flush bus-connection)
         promise)))
     ((recv mesg)
      (let ((headers (d-bus-message-headers mesg)))
        (define (maybe-resolve method)
          (let ((serial (d-bus-headers-ref headers 'REPLY_SERIAL)))
            (and=> ($ serial->ret 'ref serial #f)
                   (λ (resolve)
                     (<-np resolve method mesg)
                     ($ serial->ret 'remove serial)))))
        (let ((mesg-type (d-bus-message-type mesg)))
          (cond 
           ((= mesg-type MESSAGE_TYPE_METHOD_CALL)
            (error "Hosting objects not yet implemented"))
           ((= mesg-type MESSAGE_TYPE_METHOD_RETURN)
            (maybe-resolve 'fulfill))
           ((= mesg-type MESSAGE_TYPE_SIGNAL)
            #f)
           ((= mesg-type MESSAGE_TYPE_ERROR)
            (maybe-resolve 'break))
           (else
            (error "Invalid message type" mesg-type mesg))))))
     ((spawn-proxy path dest #:optional interface)
      (on ($ bus 'send path dest
                   "org.freedesktop.DBus.Introspectable" "Introspect" #f)
          (λ (xml-mesg)
            (spawn-named (string->symbol path)
                         ^proxy (car (d-bus-message-body xml-mesg))
                         bus path dest interface))
          #:promise? #t))
     ((%debugging-state)
      ($ serial->ret 'data))
     ((close)
      (d-bus-disconnect bus-connection))))

  (define bus (spawn ^bus))

  (define reading-thread
    (call-with-new-thread
     (lambda ()
       (let loop ()
         (<-np-extern bus 'recv (d-bus-read-message bus-connection))
         (loop)))))
  ($ bus 'send "/org/freedesktop/DBus" "org.freedesktop.DBus"
           "org.freedesktop.DBus" "Hello" #f)

  (spawn-named (string-append (ensure-string addr) "-bus")
               ^facet bus
               'send 'spawn-proxy '%debugging-state 'close))

(define concretize-addr
  (match-lambda
    ('system (d-bus-system-bus-address))
    ('session (d-bus-session-bus-address))
    ((? string? addr) addr)))

(define (^proxy bcom introspect-xml bus path dest interface)
  (define interfaces
    (if introspect-xml
        (map munge-interface
             ((sxpath '(// node interface))
              (xml->sxml introspect-xml
                         #:trim-whitespace? #t)))))

  (define ->guile
    (make-return-converter bus dest))
  (let ((path (ensure-string path))
        (dest (ensure-string dest))
        (interface (and=> interface ensure-string)))
    (define self
      (match-lambda*
        (('path) path)
        (('destination) dest)

        (((iface method (? list? signature)) args ...)
         (assert (= (length signature) (length args)))
         (on (apply <- bus 'send path dest
                    (ensure-string iface) (ensure-string method)
                    (string-join signature "") args)
             (lambda (mesg)
               (apply values
                      (map ->guile
                           (cdr ;; to skip the spurious 'message
                            (parse-type-signature
                             (d-bus-headers-ref (d-bus-message-headers mesg)
                                                'SIGNATURE)))
                           (d-bus-message-body mesg))))
             #:promise? #t
             #:catch
             (lambda (mesg)
               (error "DBus error" mesg))))
        (((iface property (? symbol? mode) . signature) args ...)
         (match args
           (()
            (self '(org.freedesktop.DBus.Properties Get ("s" "s"))
                  (ensure-string iface) (ensure-string property)))
           ((val)
            (self `(org.freedesktop.DBus.Properties Set ,(cons* "s" "s" signature))
                  (ensure-string iface) (ensure-string)))))
        (((iface member) args ...)
         (or (and-let* ((members (assoc-ref interfaces (ensure-symbol iface)))
                        (signature (assoc-ref members (ensure-symbol members))))
               (apply self (list iface member signature) args))
             (error "Invalid call or property ref for object" path dest iface member)))
        ((member args ...)
         (or (and-let* ((interfaces (ifaces-with-member member interfaces))
                        ((not (zero? (length interfaces))))
                        (iface (caar interfaces)))
               (match (cdar interfaces)
                 (((? symbol? mode) type)
                  (apply self (list iface member mode type) args))
                 ((? list? signature)
                  (apply self (list iface member signature) args))))
             (error "Invalid call or property ref for object" path dest member)))))
    self))

(define (munge-interface-elt elt)
  (sxml-match elt
    ((method (@ (name ,name))
             . ,_)
     (cons (string->symbol name)
           (filter-map
            (λ (arg)
              (sxml-match arg
                ((arg (@ (type ,type) (direction "in")))
                 type)
                (,else
                 #f)))
            ((sxpath '(arg)) elt))))
    ((property (@ (type ,type) (name ,name) (access ,access))
               . ,_)
     (list (string->symbol name) (string->symbol access) type))))

(define (munge-interface iface)
  (sxml-match iface
    ((interface (@ (name ,name)) . ,body)
     (cons (string->symbol name)
           (map munge-interface-elt
                (filter (λ (x) (member (car x) '(method property))) body))))))

(define (ifaces-with-member member ifaces)
  (filter-map (λ (iface)
                (and=> (assoc-ref (cdr iface) member)
                       (λ (mem)
                         (cons (car iface) mem))))
              ifaces))

(define (ensure-string str-or-sym)
  (cond ((symbol? str-or-sym)
         (symbol->string str-or-sym))
        ((string? str-or-sym)
         str-or-sym)))

(define (ensure-symbol sym-or-str)
  (cond ((symbol? sym-or-str)
         sym-or-str)
        ((string? sym-or-str)
         (string->symbol sym-or-str))))

(define (make-return-converter bus dest)
  (define (convert signature object)
    (match signature
      ((or 'BYTE 'BOOLEAN
           'INT16 'UINT16
           'INT32 'UINT32
           'INT64 'UINT64
           'DOUBLE
           'STRING)
       object)
      ('OBJECT_PATH
       (<- bus 'spawn-proxy object dest))
      ('SIGNATURE
       (match (parse-type-signature object)
         (('message type)
          type)
         (('message . types)
          types)))
      ;; UNIX_FD purposefully ignored

      (`(STRUCT . ,types)
       (list->vector (map convert types object)))
      (`(ARRAY (DICT_ENTRY ,keyt ,valt))
       (fold (λ (kv ghash)
               (ghash-set ghash
                          (convert keyt (car kv))
                          (convert valt (cdr kv))))
             (make-ghash)
             (vector->list object)))
      (`(ARRAY ,type)
       (map (λ (obj)
              (convert type obj))
            (vector->list object)))
      ('VARIANT
       (convert (car object) (cdr object)))
      (`(DICT_ENTRY ,keyt ,valt)
       (error "Bare dict entry"))))
  convert)

(define (call-convert signature object)
  (match signature 
    ((or 'BYTE 'BOOLEAN
         'INT16 'UINT16
         'INT32 'UINT32
         'INT64 'UINT64
         'DOUBLE
         'STRING)
     object)
    ('OBJECT_PATH
     (match object
       ((? string?) object)
       ((? live-refr?)
        (<- object 'path))))
    (`(STRUCT . ,types)
     (map call-convert types (vector->list object)))
    (`(ARRAY (DICT_ENTRY ,keyt ,valt))
     (match object
       ((? ghash?)
        (list->vector
         (ghash-fold (λ (key val acc)
                       (cons
                        (cons (call-convert keyt key)
                              (call-convert valt val))
                        acc))
                     '()
                     object)))
       (_ object)))
    (`(ARRAY ,type)
     (match object
       ((? list?)
        (list->vector
         (map (λ (obj)
                (call-convert type obj))
              object)))
       (_ object)))
    (_ object)))
