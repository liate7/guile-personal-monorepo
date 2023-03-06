(define-module (liate utils arrays)
  #:use-module (ice-9 arrays)
  #:use-module (ice-9 control)
  #:use-module (liate utils lists)
  #:use-module (pipe)
  #:export (array-indices
            array-index
            array-idx-ref
            array-idx-set array-idx-set!))

(define (array-indices array)
  "Returns a list of all valid indices of ARRAY"
  (let ((ret (array-copy array)))
    (array-index-map! ret
                      list)
    (->> ret
         (array->list)
         (flatten)
         (segment (array-rank array)))))

(define (array-index pred array)
  "Returns the first index of ARRAY where the value at that index satisfies PRED."
  (let/ec ret
   (for-each (λ (idx)
               (when (pred (array-idx-ref array idx))
                 (ret idx)))
             (array-indices array))
   #f))

(define (array-idx-ref array idx)
  "Indexes ARRAY with IDX, a list of numbers."
  (apply array-ref array idx))

(define (array-idx-set array obj idx)
  "Creates a new copy of ARRAY where the value at IDX is OBJ."
  (let ((ret (array-copy array)))
    (apply array-set! ret obj idx)
    ret))

(define (array-idx-set! array obj idx)
  "Sets the element at IDX in ARRAY to OBJ."
  (apply array-set! array obj idx))
