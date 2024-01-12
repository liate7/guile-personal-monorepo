(define-module (liate swaybar mpd)
  #:use-module (goblins)
  #:use-module (goblins actor-lib cell)
  #:use-module (goblins actor-lib methods)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 control)
  #:use-module (ice-9 match)
  #:use-module (liate goblins)
  #:use-module (liate mpd-client)
  #:use-module (liate utils assert)
  #:use-module (liate utils hashmaps)
  #:use-module (liate utils macros)
  #:use-module (guix progress)
  #:export (^mpd-block))

(define (truncate-str len str)
  (if (> (string-length str) len)
      (string-append (string-take str (1- len))
                     "…")
      str))

(define (format-current-song cur)
  (if (not cur) "none"
      (format #f "[~a] ~a~a"
              (failure-> (hashmap-ref cur 'composer)
                         (hashmap-ref cur 'album)
                         "")
              (hashmap-ref cur 'grouping
                           (const "")
                           (cute string-append <> ": "))
              (truncate-str
               60
               (failure-> (hashmap-ref cur 'title)
                          (hashmap-ref cur 'file))))))

(define* (round->int num #:optional (by 1))
  (assert (integer? by))
  (inexact->exact (round-quotient num by)))

(define* (^mpd-block bcom cell)
  (define-cell quit? #f)

  (define mpd (spawn ^mpd))

  (methods
   ((tick)
    (unless ($ quit?)
      (on (<- mpd 'begin '(status) '(current-song))
          (match-lambda
            ((status cur)
             (<- cell
                 (string-join
                  (cons* (format-current-song cur)
                         (match (hashmap-ref/default status 'state '?)
                           ('play "▶")
                           ('pause "⏸")
                           ('stop "⏹")
                           ('? "unk"))
                         (let/ec ret
                           (let* ((fail (λ () (ret "n/a")))
                                  (elapsed (hashmap-ref status 'elapsed fail))
                                  (duration (hashmap-ref status 'duration fail)))
                             (format #f "~a~a"
                                     (round->int duration 60)
                                     (progress-bar (* 100 (/ elapsed duration))
                                                   10))))
                         (match (hashmap-ref/default status 'single '?)
                           (#t (list "s"))
                           ('oneshot (list "1"))
                           (_ '())))))))
          #:catch
          (lambda (err)
            (<- cell "Error getting MPD info")))))
   ((quit)
    ($ quit? #t))))
