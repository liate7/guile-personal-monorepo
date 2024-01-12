(define-module (liate swaybar battery)
  #:use-module (goblins)
  #:use-module (goblins vat)
  #:use-module (goblins actor-lib joiners)
  #:use-module (goblins actor-lib cell)
  #:use-module (goblins actor-lib methods)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 control)
  #:use-module (ice-9 curried-definitions)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (ice-9 textual-ports)
  #:use-module (liate utils io)
  #:use-module (liate utils proc-tools)
  #:use-module (liate goblins)
  #:use-module (liate goblins dbus)
  #:export (^battery-block))

(define (^battery-block bcom cell bat urgency-threshold)
  (define-cell quit? #f)

  (methods
   ((tick)
    (unless ($ quit?)
      (on (battery-status-vow bat)
          (match-lambda
            ((% state-int to-empty to-full)
             (let* ((state (state-int->symbol state-int))
                    (in-parens
                     (match state
                       ('charging
                        (format #f "CHR, ~a" (secs->h:m-string to-full)))
                       ('discharging
                        (format #f "DIS, ~a" (secs->h:m-string to-empty)))
                       ('full "FULL")
                       ('empty "EMPTY")
                       (else "???"))))
               (<- cell
                   `((full_text
                      . ,(format #f
                                 "⚡~3d% (~a)"
                                 (inexact->exact (round %))
                                 in-parens))
                     (urgent
                      . ,(and (eq? state 'discharging)
                              (urgent-time-left? to-empty urgency-threshold))))))))
          #:catch
          (λ (err)
            (<- cell " ERROR ")))))
   ((quit)
    ($ quit? #t))))

(define (battery-status-vow bat)
  (all-of (<- bat 'Percentage) (<- bat 'State) (<- bat 'TimeToEmpty) (<- bat 'TimeToFull)))

(define state-int->symbol
  (match-lambda
    (0 #f)
    (1 'charging)
    (2 'discharging)
    (3 'empty)
    (4 'full)
    (5 '(pending charging))
    (6 '(pending discharging))
    (_ #f)))

(define (secs->h:m-string secs)
  (let* ((m (floor/ secs 60))
         (h m (floor/ m 60)))
    (format #f "~2d:~2,'0d" h m)))

(define (urgent-time-left? to-empty threshold)
  (and (not (zero? to-empty))
       (< to-empty threshold)))
