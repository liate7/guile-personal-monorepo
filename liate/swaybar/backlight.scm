(define-module (liate swaybar backlight)
  #:use-module (goblins)
  #:use-module (goblins vat)
  #:use-module (goblins actor-lib cell)
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins actor-lib joiners)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 curried-definitions)
  #:use-module (ice-9 ports)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (liate goblins)
  #:export (^backlight-block))

(define (^backlight-block bcom cell)
  (define-cell quit? #f)
  (methods
   ((tick)
    (unless ($ quit?)
      (on (all-of
           (call-with-port-vow (open-input-file
                                "/sys/class/backlight/intel_backlight/max_brightness")
                               read)
           (call-with-port-vow (open-input-file
                                "/sys/class/backlight/intel_backlight/brightness")
                               read))
          (match-lambda
            ((max cur)
             (<- cell
                 (format #f "BL ~3d%"
                         (floor/ (* 100 cur) max)))))
          #:catch
          (λ (err)
            (<- cell " ERROR ")))))
   ((quit)
    ($ quit? #t))))
