(define (make-interval a b) (cons a b))

(define (upper-bound z)
  (cdr z))

(define (lower-bound z)
  (car z))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) 
               (lower-bound y)))
        (p2 (* (lower-bound x) 
               (upper-bound y)))
        (p3 (* (upper-bound x) 
               (lower-bound y)))
        (p4 (* (upper-bound x) 
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (AND (> (upper-bound y) 0) (< (lower-bound y) 0))
    (error "dividing by 0")
    (mul-interval x 
                  (make-interval 
                    (/ 1.0 (upper-bound y)) 
                    (/ 1.0 (lower-bound y))))
    )))


(define i1 (make-interval 2 3))
(define i2 (make-interval -4 9))

;(div-interval i1 i2)
