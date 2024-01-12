(define-module (liate utils paths)
  #:use-module (oop goops)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (liate utils assert)
  #:use-module (liate utils macros)
  #:use-module (pipe)
  #:autoload (ice-9 ftw) (file-system-fold)
  #:export (parse-unix-path
            parse-path
            path->string
            ref
            path-ref))

;; Reserving the right to write Windows or other weird path implementations
(define-class <unix-path> ()
  (relative? ;; boolean
   #:getter relative?
   #:init-value #f
   #:init-keyword #:relative?)
  (force-directory? ;; boolean
   #:getter force-directory?
   #:init-value #f
   #:init-keyword #:force-directory?)
  (path ;; (list-of (or string symbol bytevector)), components \ #\\, #\nul
   #:getter pathlist
   #:init-value '()
   #:init-keyword #:path))

(define-method (write (path <unix-path>) port)
  (format port "#<unix-path #:relative? ~a #:force-directory? ~a ~s>"
          (relative? path)
          (force-directory? path)
          (pathlist path)))

(define-method (display (path <unix-path>) port)
  (format port "#<path ~a>"
          (path->string path)))

(define* (parse-unix-path string #:optional (definitely-directory? #f))
  (let* ((pathlist (string-split string #\/))
         (relative? (not (string-null? (car pathlist))))
         (force-directory? (or definitely-directory?
                               (string-null? (last pathlist)))))
    (normalize
     (make <unix-path> #:relative? relative?
           #:force-directory? force-directory?
           #:path (remove string-null? pathlist)))))

(define parse-path parse-unix-path)

(define-method (path->string (path <unix-path>))
  (format #f "~:[/~;~]~{~a~^/~}~:[~;/~]"
          (relative? path)
          (pathlist path)
          (force-directory? path)))

(define-method (.. (path <unix-path>))
  (match (list (relative? path) (pathlist path))
    ((rel? (path ... last))
     (make <unix-path> #:relative? rel? #:force-directory? #t #:path path))
    ((#t ())
     (make <unix-path> #:relative? #t #:force-directory? #t #:path '("..")))
    ((#f ())
     (error "Tried to backtrack above root"))))

(define-method (.. (path <string>))
  (.. (parse-path path)))

(define-method (leaf (path <unix-path>))
  (last (pathlist path)))

(define-method (ref (path <unix-path>) (subpath <unix-path>))
  (assert (relative? subpath))
  (make <unix-path> #:relative? (relative? path)
        #:force-directory? (force-directory? subpath)
        #:path (append (pathlist path)
                       (pathlist subpath))))

(define-method (ref (path <unix-path>) (subpath <string>))
  (ref path (parse-path subpath)))

(define-method (path-ref (path <string>) (subpath <string>))
  (ref (parse-path path) (parse-path subpath)))

(define-method (normalize (path <unix-path>))
  (make <unix-path> #:relative? (relative? path)
        #:force-directory? (force-directory? path)
        #:path (normalize-pathlist (pathlist path))))

(define (normalize-pathlist pathlist)
  (define (valid-name? name)
    (not (or (string-contains name (string #\/))
             (string-contains name (string #\nul)))))
  (match pathlist
    ((_ (or ".." '..) rest ...)
     (normalize-pathlist rest))
    (((or "." "" '|.| '||) rest ...)
     (normalize-pathlist rest))
    (((? valid-name? name) rest ...)
     (cons name (normalize-pathlist rest)))
    (() '())
    (_
     (error "Not a valid list of path components" pathlist))))

(define-method (path-exists? (path <unix-path>))
  (file-exists? (path->string path)))

(define-method (directory-files (path <unix-path>))
  #;(directory-files path #f)
  (define (enter? pathstr stat result)
    (equal? (path->string (pk path)) (pk pathstr)))
  (define (down _path _stat result)
    result)
  (define (up _path _stat result)
    result)

  (define (leaf path stat result)
    (cons (parse-path path) result))

  (define (skip path stat result)
    (cons (parse-path path #t) result))

  (file-system-fold enter? leaf down up skip error '() (path->string path)))
;; (define-method (directory-files (path <unix-path>) recursive?))

(define-method (stat-at (path <unix-path>))
  (stat (path->string path)))

(define-method (current-path)
  (parse-path (getcwd)))


;;; Or another version

(define-class <file-name> ()
  (root #:init-keyword #:root
        #:accessor root)
  (str #:init-keyword #:str
       #:init-value #f
       #:accessor %str)
  (route #:init-keyword #:route
         #:accessor route)
  (basename #:init-keyword #:basename
            #:init-value #f
            #:accessor %basename))

(define (string->file-name str)
  (let* ((str (canonicalize-file-str str))
         (segm (remove string-null? (string-split/shared str #\/))))
    (make <file-name>
      #:root (match (string-ref str 0)
               (#\/ 'root)
               (else 'relative))
      #:str str
      #:route segm
      #:basename (if (equal? (string-ref str (1- (string-length str))) #\/)
                     #f
                     (last segm)))))

(define (canonicalize-file-str str)
  (let* ((str (string-replace-substring str "//" "/"))
         (str (regexp-substitute/global #f "\\./" str
                                        'pre 'post))
         (str (regexp-substitute/global #f "[^/]+/\\.\\./" str
                                        'pre "/" 'post)))
    str))

(define (string-split/shared str char)
  (define (rec substr acc)
    (let ((idx (string-index substr char)))
      (if (and (not (string-null? substr)) idx)
          (rec (substring/shared substr (1+ idx))
               (cons (substring/shared substr 0 idx) acc))
          (reverse (cons substr acc)))))
  (rec str '()))
