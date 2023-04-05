(define-module (liate swaybar mpd)
  #:use-module (goblins)
  #:use-module (goblins actor-lib cell)
  #:use-module (goblins actor-lib methods)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 control)
  #:use-module (ice-9 match)
  #:use-module (liate goblins)
  #:use-module (liate mpd-client)
  #:use-module (liate utils macros)
  #:use-module (liate utils hashmaps)
  #:use-module (guix progress)
  #:export (^mpd-block))

(define (format-current-song cur)
  (if (not cur) "none"
      (format #f "[~a] ~a"
              (failure-> (hashmap-ref cur 'composer)
                         (hashmap-ref cur 'album)
                         "")
              (failure-> (hashmap-ref cur 'title)
                         (hashmap-ref cur 'file)))))

(define* (^mpd-block bcom cell)
  (define-cell quit? #f)

  (define mpd (spawn ^mpd))

  (methods
   ((tick)
    (on (<- mpd 'begin '(status) '(current-song))
        (match-lambda
          ((status cur)
           (<- cell
               (string-join
                (list (format-current-song cur)
                      (match (hashmap-ref/default status 'state '?)
                        ('play "▶")
                        ('pause "⏸")
                        ('stop "⏹")
                        ('? "unk"))
                      (let/ec ret
                        (let ((fail (λ () (ret "n/a"))))
                          (progress-bar (* 100 (/ (hashmap-ref status 'elapsed fail)
                                                  (hashmap-ref status 'duration fail)))
                                        10))))))))
        #:catch
        (lambda (err)
          (<- cell "Error getting MPD info"))))
   ((quit)
    ($ quit? #t))))
