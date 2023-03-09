(define-module (liate utils io)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 binary-ports)
  #:export (get-all-lines
            open-tcp-port
            port-copy))

(define (get-all-lines port)
  "Reads from PORT until an end of file, decoding characters like `get-string-n`.

If characters are available before the end of file, returns a list of all the
lines in that data.
If no character precedes the end of file, the end-of-file object is returned."
  (let ((val (get-string-all port)))
    (if (eof-object? val)
        val
        (string-split val #\newline))))

(define (open-tcp-port host port)
  "Creates an open socket connected via TCP to HOST:PORT."
  (let* ((ai (car (getaddrinfo host (number->string port)
                               ;; Have to give something, and will always be numeric
                               AI_NUMERICSERV
                               ;; Don't care between IPv{4,6}, just give me something
                               0
                               ;; For tcp
                               SOCK_STREAM (protoent:proto (getproto "tcp")))))
         (sock (socket (addrinfo:fam ai) (addrinfo:socktype ai) (addrinfo:protocol ai))))
    (connect sock (addrinfo:addr ai))
    sock))

(define (port-copy from to)
  "Copies from FROM to TO until an end of file."
  (let rec ()
    (let ((data (get-bytevector-n from 1024)))
      (unless (eof-object? data)
        (put-bytevector to data)
        (rec)))))
