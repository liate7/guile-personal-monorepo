(define-module (liate goblins)
  #:use-module (goblins)
  #:use-module (goblins actor-lib cell)
  #:use-module (goblins actor-lib methods)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-71)
  ;; #:use-module (pfds heaps)
  #:export (^notifying-cell ^timer))

(define (^notifying-cell bcom to-notify)
  (define-cell inner)
  (define self
    (case-lambda
      (() ($ inner))
      ((val)
       ($ inner val)
       (<- to-notify 'changed self))))
  self)

(define (^timer bcom)
  (define (order l r)
    (< (car l) (car r)))
  (define-cell to-notify
    (make-heap order))
  (define (wait)
    (let* ((next (heap-min ($ to-notify)))
           (time (car next)))
      (sleep time)
      (let* ((timers (map (λ (t)
                            (cons (- (car t) time)
                                  (cdr t)))
                          (heap->list ($ to-notify))))
             (finished not (partition (compose (cute <= <> 0) car) timers)))
        (for-each (compose (curry apply <-np) caddr) finished)
        ($ to-notify
           (fold (λ (item heap) (heap-insert heap (cons (cadr item) (cdr item))))
                 (list->heap not order)
                 finished)))))
  (methods
   ((register time recv . args)
    ($ to-notify (heap-insert ($ to-notify) (list time time (cons recv args))))
    (wait))
   
   ))
