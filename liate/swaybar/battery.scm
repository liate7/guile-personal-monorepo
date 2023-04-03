(define-module (liate swaybar battery)
  #:use-module (goblins)
  #:use-module (goblins vat)
  #:use-module (goblins actor-lib cell)
  #:use-module (goblins actor-lib methods)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 control)
  #:use-module (ice-9 curried-definitions)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (liate goblins)
  #:export (^battery-block))

(define (battery-status-vow)
  (on (fibrous (call-with-port (open-pipe* "r" "acpi") get-string-all))
      (λ (str)
        ;; state
        (list (cond ((string-match "Discharging" str) 'discharging)
                    ((string-match "Charging" str) 'charging)
                    ((string-match "Full" str) 'full)
                    (else #f))
              ;; Percent
              (string-trim (match:substring (string-match " [0-9]+%" str)))
              ;; Time
              (and-let* ((time (string-match "[0-9][0-9]:[0-9][0-9]:[0-9][0-9]" str))
                         (substr (match:substring time))
                         (substrs (string-split substr #\:))
                         (ret (map string->number substrs))
                         ((every identity ret)))
                ret)))
      #:promise? #t))

(define urgency-threshold (make-parameter '(0 19)))
(define (urgent-time-left? time-lst)
  (define (time-lst-<? l r)
    (let/ec ret
      (for-each (λ (l r)
                  (and=> (< l r) ret))
                l r)
      #f))
  (and time-lst
       (time-lst-<? time-lst (urgency-threshold))))

(define (^battery-block bcom cell)
  (define-cell quit? #f)
  (methods
   ((tick)
    (unless ($ quit?)
      (on (battery-status-vow)
          (match-lambda
            ((state_ % time)
             (let* ((state (match state_
                             ('charging "CHR")
                             ('discharging "DIS")
                             ('full "FULL")
                             (_else "UNK")))
                    (time-str (cond (time (format #f "~{~2d:~2,'0d~}" (drop-right time 1)))
                                    ((eq? state_ 'full) "n/a")
                                    (else "???"))))
               (<- cell
                   `((full_text .
                                ,(format #f
                                         "⚡ ~a (~a, ~a)"
                                         % state time-str))
                     (urgent . ,(urgent-time-left? time))))))))))
   ((quit)
    ($ quit? #t))))
