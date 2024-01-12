(define-module (liate swaybar)
  #:use-module (goblins)
  #:use-module (goblins actor-lib cell)
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins actor-lib joiners)
  #:use-module (goblins actor-lib facet)
  #:use-module (goblins vat)
  #:use-module (fibers)
  #:use-module (fibers conditions)
  #:use-module (fibers operations)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 curried-definitions)
  #:use-module (ice-9 match)
  #:use-module (liate goblins)
  #:use-module (liate goblins dbus)
  #:use-module (liate goblins http)
  #:use-module (json)
  #:use-module (pipe)
  #:export (run run-in-vat ^date-block dbus-object http ^block-spawner swaybar))

(define* (^date-block _bcom cell format #:optional short-format)
  (define-cell quit? #f)
  (define (only-long date)
    (date->string date format))
  (define (long-and-short date)
    `((full_text
       . ,(only-long date))
      (short_text
       . ,(date->string date short-format))))
  (let ((outputter (if short-format long-and-short only-long)))
    (methods
     ((tick)
      (unless ($ quit?)
        (<- cell
            (outputter (current-date)))))
     ((quit)
      ($ quit? #t)))))

(define stop-signal SIGUSR1)
(define header `((version . 1) (stop_signal . ,stop-signal)))

;; Format:
;; header #\newline (array-stream (array values) ...)

(define (^swaybar bcom printer finished-cond timers)
  (define-cell first-line? #t)

  (define ((quit blocks))
    "Close off the json array, stop the timers, quit the blocks, and signal are finished."
    (<- printer 'print "]\n")
    (<- printer 'close)
    (<- timers 'pause)
    (on (all-of* (map (λ (block) (<- block 'quit))
                      blocks))
        (λ (_quits)
          (fibrous
           (signal-condition! finished-cond)))))

  (define (cell-json cell)
    "Ensure that CELL is set, then convert strings to {\"full_text\": str}"
    (and=> ($ cell)
           (λ (block)
             (if (string? block)
                 `((full_text . ,block))
                 block))))

  (define (write-swaybar-json cells)
    "The actual writing process"
    (<- printer 'print
        (if ($ first-line?)
            (begin ($ first-line? #f) "")
            ",\n")
        (-> (map cell-json cells)
            (list->vector)
            (scm->json-string #:pretty #f))))

  (define (initialize! cells blocks)
    (<- printer 'print
        (scm->json-string header)
        "\n[\n")

    (define initializing-beh
      (methods
       ((changed _ignore)
        (when (every $ cells)
          (bcom initialized-beh)))
       ((block-values)
        (map $ cells))
       (quit (quit blocks))))

    (define initialized-beh
      (methods
       ((changed _ignore)
        (write-swaybar-json cells))
       ((block-values)
        (map $ cells))
       (quit (quit blocks))))
    initializing-beh)

  (methods
   ((init cells blocks)
    (bcom (initialize! cells blocks)))
   ((changed _ignore)
    #f)
   (quit (quit '()))))

(define-record-type <dbus-object>
  (dbus-object bus destination path)
  dbus-object?
  (bus dbus-object-bus)
  (destination dbus-object-destination)
  (path dbus-object-path))

(define http (list 'http))
(define (http-object? obj)
  (eq? obj http))

(define (^block-spawner bcom timers system-bus session-bus http)
  (define hydrate-dbus-object
    (match-lambda
      ((? dbus-object?
          (= dbus-object-bus bus)
          (= dbus-object-destination dest)
          (= dbus-object-path path))
       (match bus
         ('system
          (<- system-bus 'spawn-proxy path dest))
         ('session
          (<- session-bus 'spawn-proxy path dest))))
      ((? http-object?)
       http)
      (obj obj)))
  (methods
   ((spawn cell name ctor secs #:rest args)
    (define block
      (apply spawn-named name ctor cell
             (map hydrate-dbus-object args)))
    (<- timers 'register 0 secs block 'tick)
    block)))

(define-syntax-rule (swaybar (name ctor secs args ...) ...)
  (run (list (list 'name ctor secs args ...) ...)
       (current-output-port)))

(define (run block-specs port)
  (define vat (spawn-vat))
  (let ((bar finished-cond timers
             (with-vat vat
                       (run-in-vat block-specs port))))
    (sigaction stop-signal
      (λ (sig)
        (<-np-extern timers 'pause))
      0)
    (sigaction SIGCONT
      (λ (sig)
        (<-np-extern timers 'resume))
      0)
    (sigaction SIGINT
      (λ (sig)
        (restore-signals)
        (<-np-extern bar 'quit))
      0)
    (perform-operation (wait-operation finished-cond))))

(define (run-in-vat block-specs port)
  (define finished-cond (make-condition))

  (define-values (timers timers-admin)
    (spawn-timers))

  (define bar
    (spawn ^swaybar (spawn ^printer port) finished-cond
           timers-admin))

  (define cells
    (map (λ __ (spawn ^notifying-cell bar)) block-specs))

  (define spawner
    (spawn ^block-spawner timers
           (spawn-bus 'system)
           (spawn-bus 'session)
           (spawn ^http-manager)))

  (define blocks
    (map (λ (spec cell)
           (apply $ spawner 'spawn cell spec))
         block-specs
         cells))

  ($ bar 'init cells blocks)
  (values bar finished-cond timers-admin))
