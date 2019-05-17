(define (make-interval a b) (cons a b))

(define (upper-bound z)
  (cdr z))

(define (lower-bound z)
  (car z))


(define (make-center-percentage c p)
  (make-interval (* c (- 1 (/ p 100))) (* c (+ (/ p 100) 1))))

(define (center i)
  (/ (+ (lower-bound i) 
        (upper-bound i)) 
     2))

(define (percent z)
  (/ (- (cdr z) (car z)) (* 2 (center z))))

(define i1 (make-center-percentage 2 20))

;(display (upper-bound i1))
;(display (lower-bound i1))
