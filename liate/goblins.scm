(define-module (liate goblins)
  #:use-module ((fibers) #:select (sleep))
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
  #:export (^notifying-cell ^timers))

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

(define (^timers bcom)
  (define (timer-expires-after? time timer)
    (< (if (number? time)
           time
           (timer-fires-after time))
       (timer-fires-after timer)))

  (define-cell to-notify '())
  (define-cell paused?)

  (define (on-timeout . _ignore)
    (unless ($ paused?)
      (and-let* ((wheel ($ to-notify))
                 ((not (null? wheel)))
                 (now (current-time))
                 ((not (timer-expires-after? now (car wheel)))))
        (let ((done not-yet (span (negate (cute timer-expires-after? now <>)) wheel)))
          ($ to-notify
             (sort (append
                    (filter timer?
                            (map (λ (timer)
                                   ((timer-notifier timer))
                                   (if (timer-repeats-every timer)
                                       (set-timer-fires-after
                                        timer
                                        (+ (timer-repeats-every timer)
                                           now))
                                       #f))
                                 done))
                    not-yet)
                   timer-expires-after?))))
      (wait)))

  (define (wait)
    (unless (or (null? ($ to-notify))
                ($ paused?))
      (on (timeout-vow (min 1 (- (timer-fires-after
                                  (car ($ to-notify)))
                                 (current-time))))
          #:catch pk
          #:finally on-timeout)))

  (define (register first repeat recv . args)
    ($ to-notify (sort (cons (make-timer (+ (current-time) first)
                                         repeat
                                         (if (procedure? recv)
                                             (λ () (apply recv args))
                                             (λ () (apply <-np recv args))))
                             ($ to-notify))
                       timer-expires-after?))
    (wait))

  (methods
   (register register)

   ((pause) ($ paused? #t))
   ((paused?) ($ paused?))
   ((resume)
    ($ paused? #f)
    (on-timeout))))
