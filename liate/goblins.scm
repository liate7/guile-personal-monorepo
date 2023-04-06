(define-module (liate goblins)
  #:use-module ((fibers) #:select (sleep spawn-fiber))
  #:use-module (fibers channels)
  #:use-module (goblins)
  #:use-module (goblins actor-lib cell)
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins vat)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 match)
  #:use-module (liate utils assert)
  #:use-module (liate utils proc-tools)
  #:use-module (pipe)
  #:export (^notifying-cell call-with-port-vow spawn-timers ^printer))

(define* (^notifying-cell bcom to-notify #:optional val)
  (define-cell inner)
  (when val
    (inner val))
  (define self
    (case-lambda
      (() ($ inner))
      ((val)
       ($ inner val)
       (<- to-notify 'changed self))))
  self)

(define (call-with-port-vow port proc)
  (on (fibrous
       (proc port))
      #:promise? #t
      #:finally (λ () (close-port port))))

(define (timeout-vow secs)
  (fibrous
   (let ((start (current-time)))
     (sleep secs)
     (- (current-time) start))))

(define-immutable-record-type <timer>
  (make-timer fires-after repeats-every to-notify)
  timer?
  (fires-after timer-fires-after set-timer-fires-after)
  (repeats-every timer-repeats-every)
  (to-notify timer-notifier))

(define (timer-expires-after? time timer)
  (< (if (number? time)
         time
         (timer-fires-after time))
     (timer-fires-after timer)))

(define (update-timers done not-yet now)
  (define (on-done timer)
    ((timer-notifier timer))
    (and=> (timer-repeats-every timer)
           (λ (repeat)
             (set-timer-fires-after
              timer
              (+ repeat now)))))
  (-> (filter-map on-done done)
      (append not-yet)
      (sort timer-expires-after?)))

(define (spawn-timers)
  (define-cell to-notify '())

  (define (^state-machine bcom)
    (define (on-timeout)
      (and-let* ((wheel ($ to-notify))
                 ((not (null? wheel)))
                 (now (current-time))
                 ((not (timer-expires-after? now (car wheel)))))
        (let ((done not-yet (span (negate (cute timer-expires-after? now <>)) wheel)))
          ($ to-notify
             (update-timers done not-yet now))))
      (<- self 'wait)
      (bcom base-beh))

    (define base-beh
      (methods
       ((wait)
        (unless (null? ($ to-notify))
          (on (timeout-vow (min 1 (- (timer-fires-after
                                      (car ($ to-notify)))
                                     (current-time))))
              #:catch pk
              #:finally (λ () ($ self 'on-timeout))))
        (bcom waiting-beh))
       (on-timeout on-timeout)

       ((pause) (bcom paused-beh))
       ((state) 'base)
       ((resume) (values))))
    (define waiting-beh
      (methods
       ((wait) #t)
       (on-timeout on-timeout)

       ((pause) (bcom paused-beh))
       ((state) 'waiting)
       ((resume) (values))))
    (define paused-beh
      (methods
       ((wait) #t)
       ((on-timeout) #f)

       ((pause) (values))
       ((state) 'paused)
       ((resume)
        (<- self 'wait)
        (bcom base-beh))))
    base-beh)
  
  (define self (spawn ^state-machine))

  (define (register first repeat recv . args)
    (assert (integer? first)
            ((proc-or integer? not) repeat)
            ((proc-or procedure? local-refr? far-refr?) recv))
    (let* ((notify (if (procedure? recv)
                       (if (null? args) recv
                           (λ () (apply recv args)))
                       (λ () (apply <-np recv args))))
           (timer (make-timer (+ (current-time) first)
                              repeat
                              notify)))
      ($ to-notify
         (sort (cons timer
                     ($ to-notify))
               timer-expires-after?))
      ($ self 'wait)
      timer))
  (define (unregister timer)
    ($ to-notify
       (remove (λ (t)
                 (eq? (timer-notifier timer)
                      (timer-notifier t)))
               ($ to-notify))))
  
  (define (^registrar bcom)
    (methods
     (register register)
     (unregister unregister)))
  (define (^admin bcom)
    (methods
     (register register)
     (unregister unregister)
     
     ((pause)
      ($ self 'pause))
     ((paused?)
      (eq? ($ self 'state) 'paused))
     ((resume)
      ($ self 'resume))
     ((%debugging-state)
      (list ($ self 'state) ($ to-notify)))))

  (values (spawn ^registrar) (spawn ^admin)))

(define* (^printer bcom port #:optional (owned? #t))
  (define chan (make-channel))
  (define fiber
    (spawn-fiber
     (λ ()
       (let rec ((mesg (get-message chan)))
         (match mesg
           ('close
            (unless (not owned?)
              (close-port port)))
           (('print str)
            (put-string port str)
            (force-output port)
            (rec (get-message chan))))))))

  (methods
   ((print #:rest strs)
    (fibrous
     (put-message chan (list 'print (apply string-append strs)))
     #t))
   ((close)
    (bcom (λ (mtd . args)
            (if (eq? mtd 'closed?) #t
                (error "Invalid method (printer closed" mtd args)))
          (fibrous
           (put-message chan 'close)
           #t)))
   ((closed?) #f)))
