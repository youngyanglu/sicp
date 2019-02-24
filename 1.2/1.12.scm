(define (pascal-at-coord x y)
  (cond ((= y 1) 1)
        ((and (= x 2) (= y 2)) 2)
        ((= y (+ 1 x)) 1)
        (else (+ (pascal-at-coord (- x 1) (- y 1)) (pascal-at-coord (- x 1) y)))
        ))

;(pascal-at-coord 6 4)

