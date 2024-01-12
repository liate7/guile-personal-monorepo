(define-module (liate utils math)
  #:export (sign clamp))

(define (sign num)
  "Returns 1 if NUM > 0, -1 if NUM < 0, or 0 if NUM is 0"
  (cond ((> num 0)   1)
        ((< num 0)  -1)
        ((zero? num) 0)))

(define (clamp lower n higher)
  (max (min n higher) lower))
