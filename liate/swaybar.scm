(define-module (liate swaybar)
  #:use-module (ice-9 curried-definitions)
  #:use-module (goblins)
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins actor-lib cell)
  #:export (run))

(define date-format
  (make-parameter "~1 (~a, UTC~z) ~3"))

(define (^date-block bcom cell timer)              ; …
  (define self
    (methods
     ((tick)
      (<- cell
          `((full_text
             . ,(date->string (current-date)
                              (date-format))))))
     ((quit))))
  (<- timer 'register (seconds 1) self 'tick)
  self)

(define (^swaybar bcom port vat)
  (define (initialized-beh cells blocks)
    (methods
     ((changed _ignore)
      #;(render …))
     ((quit)
      (for-each (λ (block) (<- block 'quit))
                blocks)
      (vat 'halt))))

  (methods
   ((init cells blocks)
    (bcom (initialized-beh cells blocks)))
   ((changed _ignore)
    "Not initialized yet")))

(define ((run-in-vat block-ctors vat))
  (define bar
    (spawn ^swaybar (current-output-port) vat))

  (define cells
    (map (λ __ (spawn ^notifying-cell bar)) block-ctors))

  (define blocks
    (map (λ (ctor cell)
           (spawn ctor cell))
         block-ctors
         cells))

  ($ bar 'init cells blocks))

(define (run block-ctors)
  (define vat (spawn-vat))
  (vat (run-in-vat block-ctors vat)))
