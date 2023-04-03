(define-module (liate goblins)
  #:use-module ((fibers) #:select (sleep))
  #:use-module (goblins)
  #:use-module (goblins actor-lib cell)
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins vat)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
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

(define (^timers bcom)
  (define (timer-expires-after? time after)
    (< (car time) (car after)))

  (define-cell to-notify '())
  (define-cell paused?)

  (define (on-timeout . _ignore)
    (unless ($ paused?)
      (and-let* ((wheel ($ to-notify))
                 ((not (null? wheel)))
                 (now (current-time))
                 ((not (timer-expires-after? (list now) (car wheel)))))
        (let ((done not-yet (span (negate (cute timer-expires-after? (list now) <>)) wheel)))
          ($ to-notify
             (sort (append (map (λ (timer)
                                  (apply <-np (list-ref timer 2))
                                  (cons (+ now (cadr timer)) (cdr timer)))
                                done)
                           not-yet)
                   timer-expires-after?))))
      (wait)))

  (define (wait)
    (unless ($ paused?)
      (on (timeout-vow (min 1 (- (caar (if (null? ($ to-notify)) 1
                                           ($ to-notify)))
                                 (current-time))))
          #:catch pk
          #:finally on-timeout)))

  (define (register time recv . args)
    ($ to-notify (sort (cons (list (+ (current-time) time) time (cons recv args))
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
