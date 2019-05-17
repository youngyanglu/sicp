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

(define (make-rectangle l h w)
  (cons l (cons h w))
  )

(define (left-top-rect r)
  (car r))

(define (height-width-rect r)
  (cdr r))

(define (make-rectangle-2 tl br)
  (print-point tl)
  (print-point br)
  (cons tl (cons (abs (- (y-point tl) (y-point br))) (abs (- (x-point tl) (x-point br))))))

(define (area-rect r)
  (* (car (height-width-rect r)) (cdr (height-width-rect r))))

(define (peri-rect r)
  (* 2 (+ (car (height-width-rect r)) (cdr (height-width-rect r)))))

(define left-top (make-point 3 5))
(define right-bottom (make-point 5 10))
;(define rect (make-rectangle left-top 2 3))
(define rect (make-rectangle-2 left-top right-bottom))
(display (car (car rect)))

(area-rect rect)
(peri-rect rect)
