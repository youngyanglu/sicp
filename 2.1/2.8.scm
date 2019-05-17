(define (make-interval a b) (cons a b))

(define (upper-bound z)
  (cdr z))

(define (lower-bound z)
  (car z))

(define (sub-interval i1 i2)
  (make-interval (- (lower-bound i1) (upper-bound i2)) (- (upper-bound i1) (lower-bound i2))))

(define i1 (make-interval 2 3))
(define i2 (make-interval 4 9))

;(define diff (sub-interval i1 i2))
