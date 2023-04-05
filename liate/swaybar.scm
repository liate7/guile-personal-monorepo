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
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 curried-definitions)
  #:use-module (liate goblins)
  #:use-module (json)
  #:use-module (pipe)
  #:export (run run-in-vat ^date-block ^block-spawner swaybar))

(define date-format
  (make-parameter "~1 (~a, UTC~z) ~3"))

(define (^block-spawner bcom timers)
  (methods
   ((spawn cell name ctor secs)
    (define block (spawn-named name ctor cell))
    (<- timers 'register secs block 'tick)
    block)))

(define (^date-block _bcom cell)
  (define-cell quit? #f)
  (methods
   ((tick)
    (unless ($ quit?)
      (<- cell
          (date->string (current-date)
                        (date-format)))))
   ((quit)
    ($ quit? #t))))

(define header '((version . 1)))

;; Format:
;; header #\newline (array-stream (array values) ...)

(define (^swaybar bcom port finished-cond pauser)
  (define (quit blocks)
    (format port "]")
    (<- pauser 'pause)
    (on (all-of* (map (λ (block) (<- block 'quit))
                      blocks))
        (λ (_quits)
          (fibrous
           (signal-condition! finished-cond)))))

  (define (initialized-beh cells blocks)
    (scm->json header port)
    (format port "~%[~%")
    (force-output port)

    (for-each (λ (block)
                (<- block 'tick))
              blocks)

    (methods
     ((changed _ignore)
      (when (port-closed? port)
        (quit blocks))
      (unless (any (compose not $) cells)
        (-> (map (λ (val)
                   (and=> ($ val)
                          (λ (block)
                            (if (string? block)
                                `((full_text . ,block))
                                block))))
                 cells)
            (list->vector)
            (scm->json port #:pretty #f))
        (format port ",~%")
        (force-output port)))
     ((block-values)
      (map $ cells))
     (quit (quit blocks))))

  (methods
   ((init cells blocks)
    (bcom (initialized-beh cells blocks)))
   ((changed _ignore)
    (format (current-error-port) "Not initialized yet~%"))
   (quit (quit '()))))

(define (run-in-vat block-specs)
  (define finished-cond (make-condition))

  (define timers (spawn ^timers))

  (define bar
    (spawn ^swaybar (current-output-port) finished-cond
           (spawn ^facet timers 'pause)))

  (define cells
    (map (λ __ (spawn ^notifying-cell bar)) block-specs))

  (define spawner (spawn ^block-spawner timers))

  (define blocks
    (map (λ (spec cell)
           (apply $ spawner 'spawn cell spec))
         block-specs
         cells))

  ($ bar 'init cells blocks)
  (values bar finished-cond))

(define log-file
  (make-parameter "/home/liate/.log/swaybar.scm.log"))

(define (run block-specs)
  (define vat (spawn-vat))
  (call-with-output-file (log-file)
    (λ (port)
      (parameterize ((current-error-port port))
        (let ((bar finished-cond
                   (with-vat vat
                             (run-in-vat block-specs))))
          (sigaction SIGINT
            (λ (sig)
              (restore-signals)
              (with-vat vat (<- bar 'quit))))
          (perform-operation (wait-operation finished-cond)))))))

(define-syntax-rule (swaybar (name ctor secs args ...) ...)
  (run (list (list 'name ctor secs args ...) ...)))
