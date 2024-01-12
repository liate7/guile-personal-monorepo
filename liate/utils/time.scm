(define-module (liate utils time)
  #:use-module (srfi srfi-19)
  #:export (hours minutes seconds
                  iso-8601-string->date))

(define (hours n)
  (* 3600 n))
(define (minutes n)
  (* 60 n))
(define (seconds n)
  n)

(define (iso-8601-string->date str)
  "Converts a fully specified ISO 8601 date string into a SRFI 19 date object.
Necessary because STRING->DATE gets confused by colons in the zone part."
  (let* ((zone-start (string-rindex str (char-set #\+ #\- #\Z #\z))))
	(string->date
	 (string-append (string-take str zone-start)
					(string-delete #\: str zone-start))
	 "~Y-~m-~dT~H:~M:~S~z")))
