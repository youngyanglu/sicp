(define (make-point x y)
  (cons x y)
)

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment s e)
  (cons s e))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (print-segment s)
  (newline)
  (print-point (start-segment s))
  (display ",")
  (print-point (end-segment s))
  )

(define (midpoint-segment s)
  (make-point 
  (/ (+ (x-point (start-segment s)) (x-point (end-segment s))) 2)
  (/ (+ (y-point (start-segment s)) (y-point (end-segment s))) 2)
  ))

(define start (make-point 3 5))
(define end (make-point 5 10))
(define segment (make-segment start end))
;(print-segment segment)
(print-point (midpoint-segment segment))
