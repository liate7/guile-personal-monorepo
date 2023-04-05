(define-module (liate mpd-client)
  #:use-module (ice-9 curried-definitions)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 format)

  #:use-module (srfi srfi-1) ; lists
  #:use-module (srfi srfi-2) ; and-let*
  #:use-module (srfi srfi-9) ; records
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-19) ; dates
  #:use-module (srfi srfi-26) ; cut & friends

  #:use-module (goblins)
  #:use-module (goblins vat)

  #:use-module (liate utils assert)
  #:use-module (liate utils io)
  #:use-module (liate utils proc-tools)
  #:use-module (liate utils hashmaps)

  #:export (make-mpd
            ^mpd
            ;; commands export themselves
            mpd-host mpd-port))

;;;; Data type

(define-immutable-record-type <mpd>
  (%make-mpd host port)
  mpd?
  (host mpd-host)
  (port mpd-port))

(define *mpd-default-host*
  (or (getenv "MPD_HOST") "localhost"))

(define *mpd-default-port*
  (or (and=> (getenv "MPD_PORT") string->number)
      6600))

(define* (make-mpd #:key (host *mpd-default-host*) (port *mpd-default-port*))
  (%make-mpd host port))

;;;; Write to the socket

(define (mpd-open-port mpd)
  (open-tcp-port (mpd-host mpd) (mpd-port mpd)))

(define (call-with-open-mpd-port mpd proc)
  (call-with-port (mpd-open-port mpd)
    (λ (port)
      (assert (string-prefix? "OK MPD " (get-line port)))
      (proc port))))

;;;; Command helpers

;;; Type procedures
;; A type procedure is a function that, given a value, returns either
;; #f (if the value is not a valid member of the type), or
;; a string that represents the value such as can be sent to mpd.

(define* (string-escape-chars str char-set #:optional (escape #\\))
  (list->string
   (reverse
    (string-fold (lambda (char acc)
                   (if (char-set-contains? char-set char)
                       (cons* char #\\ acc)
                       (cons char acc)))
                 '()
                 str))))
(define* (mpd-escape-string str #:optional wrap?)
  (let ((ret (string-escape-chars str (char-set #\\ #\' #\")))
        (quote (if (char? wrap?) wrap? "\"")))
    (if wrap?
        (string-append quote ret quote)
        ret)))

(define *mpd-known-tags*
  '((artist "artist" sort musicbrainz-id)
    (album "album" sort musicbrainz-id)
    (album-artist "albumartist" sort musicbrainz-id)
    (title "title" sort)
    (track "track" musicbrainz-id)
    (name "name")
    (genre "genre")
    (mood "mood")
    (date "date")
    (original-date "originaldate")
    (composer "composer" sort)
    (performer "performer")
    (conductor "conductor")
    (work "work" musicbrainz-id)
    (ensemble "ensemble")
    (movement "movement")
    (movement-number "movementnumber")
    (location "location")
    (grouping "grouping")
    (comment "comment")
    (disc "disc")
    (label "label")))

(define (tag-type tag)
  (define (modified-tag-type tag modifier)
    (and-let* ((tag-val (assoc-ref *mpd-known-tags* tag))
               ((member modifier tag-val)))
      (if (eq? modifier 'musicbrainz-id)
          (string-append "musicbrainz_" (car tag-val) "id")
          (string-append (car tag-val) (symbol->string modifier)))))
  (match tag
    ;; This only is recognized as a musicbrainz id, not in general
    ('(musicbrainz-id release-track)
     "musicbrainz_releasetrackid")
    ((modify tag)
     (modified-tag-type tag modify))
    (tag
     (and=> (assoc-ref *mpd-known-tags* tag) car))))

(define (date-type val)
  (and (string? val) (mpd-escape-string val)))

(define (find-expression-type expr)
  "Converts a schemified find expression into an mpd string.

Available queries include:
(tag-equal? TAG VAL)
(tag-contains? TAG VAL)
(tag-prefix? TAG VAL)
(tag-match? TAG VAL)
Tests whether the tag named TAG equals, contains, is prefixed by, or regex-matches VAL,
respectively.  TAG can also be the symbol `any`, in which case all tags are checked.
As with mpd, `album-artist` falls back on `artist` if it does not exist.

(file-equal? FILENAME)
(file-base? DIRNAME)
(file-modified-since? DATE)
`file-equal?` matches the file whose path relative to the music directory is FILENAME.
`file-base?` matches any file within the directory DIRNAME.
`file-modified-since?` matches any file modified after the ISO 8601 date DATE.

(audio-format-equals? SAMPLE-RATE BITS CHANNELS)
(audio-format-matches? SAMPLE-RATE BITS CHANNELS)
`audio-format-equals?` matches the fully specified audio format SAMPLE-RATE:BITS:CHANNELS;
`audio-format-matches?` does the same, but with a mask value (like the above, but any of
the attributes may be `*` to match any value of the attribute).

(priority>=? PRIORITY)
`priority>=?` matches any any queued piece with a priority higher than PRIORITY.

(not EXPR)
(and EXPR1 EXPR2 . EXPRS)
`not` negates the contained expression, and
`and` matches iff all of the subexpressions match."
  (define (tag-or-any-type tag)
    (if (eq? tag 'any) "any"
        (tag-type tag)))
  (define (af-or-mask? val)
    (or (number? val)
        (equal? val '*)))

  (define compile
    (match-lambda
      (('tag-equal? (? tag-or-any-type tag) (? string? val))
       (format #f "(~a == '~a')" (tag-or-any-type tag) (mpd-escape-string val)))
      (('tag-contains? (? tag-or-any-type tag) (? string? val))
       (format #f "(~a contains '~a')" (tag-or-any-type tag) (mpd-escape-string val)))
      (('tag-prefix? (? tag-or-any-type tag) (? string? val))
       (format #f "(~a starts_with '~a')" (tag-or-any-type tag) (mpd-escape-string val)))
      (('tag-match? (? tag-or-any-type tag) (? string? val))
       (format #f "(~a =~~ '~a')" (tag-or-any-type tag) (mpd-escape-string val)))
      (('file-equal? (? string? val))
       (format #f "(file == '~a')" (mpd-escape-string val)))
      (('file-base? (? string? val))
       (format #f "(base '~a')" (mpd-escape-string val)))
      (('file-modified-since? (? date-type date))
       (format #f "(modified-since '~a')" (date-type date)))
      (('audio-format-equals? (? number? sample-rate) (? number? bits) (? number? channels))
       (format #f "(AudioFormat == '~a:~a:~a')" sample-rate bits channels))
      (('audio-format-matches? (? af-or-mask? sample-rate)
                               (? af-or-mask? bits)
                               (? af-or-mask? channels))
       (format #f "(AudioFormat =~~ '~a:~a:~a')" sample-rate bits channels))
      (('priority>=? (? number? prio))
       (format #f "(prio >= ~a)" prio))
      (('not expr)
       (format #f "(!~a)" (compile expr)))
      (('and expr1 expr2 expr* ...)
       (format #f "(~a AND ~a~{ AND ~a~})"
               (compile expr1) (compile expr2) (map compile expr*)))
      (_else #f)))
  (and=>
   (compile expr)
   (cute mpd-escape-string <> #t)))

(define (boolean-type obj)
  (match obj
    ((or 1 #t) "1")
    ((or 0 #f) "0")))

(define (oneshot-status-type obj)
  (or (boolean-type obj)
      (if (eq? obj 'oneshot) "oneshot"
          #f)))

(define (crossfade-type obj)
  (match obj
    (#f "0")
    ((? number?) (number->string obj))))

(define (integer-type num)
  (and (integer? num)
       (number->string num)))

(define (positive-integer-type num)
  (and-let* ((str (integer-type num))
             ((> num 0)))
    str))

(define integer-range-type
  (case-lambda
    ((max)
     (integer-range-type 0 max))
    ((min max)
     (lambda (num)
       (and (integer? num)
            (<= min num max)
            (number->string num))))))

(define playlist-position-type positive-integer-type)
(define playlist-id-type positive-integer-type)

(define (time-type num)
  (and (rational? num)
       (number->string (exact->inexact num))))

;;; Return value readers
;; Take in a port, should either return a value, if the data on the port
;; describes successful execution, or
;; throw an exception if data on the port describes a failed command or
;; are invalid.

(define (key-name? str)
  (and (> (string-length str) 1)
       (equal? (string-take-right str 1) ":")))

(define (get-binary port n)
  (let ((ret (get-bytevector-n port n)))
    (assert (string-null? (get-line port)))
    ret))

(define (get-key-value str)
  ((juxt (compose string->symbol
                  (curry string-take str))
         (λ (i) (string-drop str (+ 2 i))))
   (string-index str #\:)))

(define* (remove-keyword-args lst #:optional keys)
  (let ((to-remove? (if keys (cute member <> keys) keyword?)))
    (let rec ((lst lst)
              (acc '()))
      (cond ((null? lst)
             (reverse acc))
            ((to-remove? (car lst))
             (rec (cddr lst) acc))
            (else
             (rec (cdr lst) (cons (car lst) acc)))))))

(define (throw-mpd-error mesg-list command error+index)
  (error (string-join mesg-list " ") command error+index))

(define* ((read-single-key-value #:key mapper (success-delimiter "OK")) port)
  (define (default-adder map k-v)
    (apply hashmap-set map k-v))

  (define ((alist-adder alist) map k-v)
    (match-let* (((key val) k-v)
                 (val-mod identity)
                 ((or ((? procedure? val-mod))
                      ((? symbol? key))
                      (key val-mod)
                      #f)
                  (assoc-ref alist key)))
      (hashmap-set map key (val-mod val))))

  (define (mapper->adder mapper)
    (match mapper
      (#f default-adder)
      ((? list?)
       (alist-adder mapper))))

  (let ((adder (mapper->adder mapper)))
    (let rec ((response (hashmap)))
      (let ((line (get-line port)))
        (if (eof-object? line)
            (error "EOF")
            (match (string-split line char-set:whitespace)
              ((success-delimiter)
               (if (hashmap-empty? response) #f response))
              (("ACK" error+index command . message)
               (throw-mpd-error message command error+index))
              (("binary:" (= string->number size))
               (error "Binary not a key-value"))
              (((? key-name?) . _rest)
               (rec (adder response (get-key-value line))))
              (_else
               (error "Invalid response line"))))))))

(define (read-boolean str)
  (match str
    ("0" #f)
    ("1" #t)))

(define (read-oneshot-status str)
  (match str
    ("0" #f)
    ("1" #t)
    ("oneshot" 'oneshot)))

(define (read-audio-format str)
  (map string->number (string-split str #\:)))

(define read-status
  (compose (cute hashmap-delete <> 'time)
           (read-single-key-value
            #:mapper `((volume ,string->number)
                       (repeat ,read-boolean)
                       (random ,read-boolean)
                       (single ,read-oneshot-status)
                       (consume ,read-oneshot-status)
                       (playlist queue-revision ,string->number)
                       (playlistlength playlist-length ,string->number)
                       (state ,string->symbol)
                       (song song-index ,string->number)
                       (songid song-id ,string->number)
                       (next-song next-song-index ,string->number)
                       (next-songid next-song-id ,string->number)
                       (elapsed ,string->number)
                       (duration ,string->number)
                       (bitrate ,string->number)
                       (xfade crossfade ,string->number)
                       (mixrampdb mixramp-threshold ,string->number)
                       (mixrampdelay mixramp-delay ,string->number)
                       (audio audio-format ,read-audio-format)))))

(define read-song
  (compose (cute hashmap-delete <> 'Time)
           (read-single-key-value
            #:mapper `((AlbumArtist album-artist)
                       (Composer composer)
                       (Title title)
                       (Last-Modified last-modified ,(cute string->date <> "~Y-~m-~dT~H:~M:~SZ"))
                       (Label label)
                       (Album album)
                       (Genre genre)
                       (Id song-id ,string->number)
                       (Format audio-format ,read-audio-format)
                       (duration ,string->number)
                       (Artist artist)
                       (Track track ,string->number)
                       (Pos pos ,string->number)))))

(define (read-success port)
  (match (string-split (get-line port) char-set:whitespace)
    (("OK") #t)
    (("ACK" error+index command . message)
     (throw-mpd-error message command error+index))
    (((? key-name?) . _rest)
     (error "Unexpected key-value"))
    (_else
     (error "Invalid response line"))))

(define ((read-tag name conv) port)
  (let ((line (get-line port)))
    (if (equal? line "OK") #f
        (and-let* ((kv (get-key-value line))
                   ((eq? name (car kv))))
          (read-success port)
          (conv (cadr kv))))))

;;;; Goblins object

(define commands-table
  (make-hash-table))

(define* (^mpd bcom #:key (host *mpd-default-host*) (port *mpd-default-port*))
  (define mpd (make-mpd #:host host #:port port))

  (define (get-command name args)
    (let ((proc (hash-ref commands-table name)))
      (when (not proc)
        (error "Not a valid command ~s" name))
      (apply proc args)))

  (match-lambda*
    (('begin (cmds . args) ...)
     (let ((body-procs (map get-command cmds args)))
       (fibrous
        (call-with-open-mpd-port mpd
          (λ (port)
            (let rec ((procs body-procs))
              (if (null? procs) procs
                  (cons ((car procs) port)
                        (rec (cdr procs))))))))))
    ((cmd . args)
     (let ((proc (get-command cmd args)))
       (fibrous
        (call-with-open-mpd-port mpd
          proc))))))

;;;; Commands

(define ((make-mpd-command wire-name arg-convertors recv) . args)
  "Creates a procedure which, when called with an open port
(presumably connected to an MPD instance),
sends the given command on the port and reads the response into a result."
  (define (error-if-false-result proc arg)
    (let ((res (proc arg)))
      (when (not res)
        (error (format #f "Invalid argument to ~a command" wire-name) arg))
      (assert (string? res))
      res))

  (assert (= (length args) (length arg-convertors)))
  (lambda (port)
    (format port "~a~%"
            (string-join (cons (symbol->string wire-name)
                               (map error-if-false-result arg-convertors args))
                         " "))
    (recv port)))

(define-syntax define-command
  (lambda (s)
    (syntax-case s ()
      ((define-command ((name wire-name) (args ->string-s) ...) recv)
       (let ((proc-name (datum->syntax s (symbol-append 'mpd- (syntax->datum #'name)))))
         #`(define-public #,proc-name
             (let ((proc (make-mpd-command 'wire-name (list ->string-s ...) recv)))
               (assert (not (hash-ref commands-table 'name)))
               (hash-set! commands-table 'name proc)
               (lambda (mpd args ...)
                 (call-with-open-mpd-port mpd
                   (proc args ...)))))))
      ((define-command (name (args ->string-s) ...) recv)
       (let ((proc-name (datum->syntax s (symbol-append 'mpd- (syntax->datum #'name)))))
         #`(define-public #,proc-name
             (let ((proc (make-mpd-command 'name (list ->string-s ...) recv)))
               (assert (not (hash-ref commands-table 'name)))
               (hash-set! commands-table 'name proc)
               (lambda (mpd args ...)
                 (call-with-open-mpd-port mpd
                   (proc args ...))))))))))

;;; Status

(define-command (status) read-status)

(define-command ((current-song currentsong)) read-song)

(define-command ((clear-error clearerror)) read-success)

(define-command (stats) (read-single-key-value))

;;; Playback option

(define-command (consume (state oneshot-status-type)) read-success)

(define-command (crossfade (seconds crossfade-type)) read-success)

(define-command (random (state boolean-type)) read-success)

(define-command (repeat (state boolean-type)) read-success)

(define-command ((set-volume setvol) (vol (integer-range-type 100))) read-success)
(define-command ((get-volume getvol)) (read-tag 'volume string->number))

(define-command (single (state oneshot-status-type)) read-success)

;;; Playback control

(define-command (next) read-success)
(define-command (previous) read-success)

(define-command (pause (state boolean-type)) read-success)
(define-command ((toggle pause)) read-success)

(define-command ((play playid) (piece playlist-id-type)) read-success)

(define-command ((seek seekid) (piece playlist-id-type) (time time-type)) read-success)

(define-command (stop) read-success)

;;; Queue

(define-command (clear) read-success)
