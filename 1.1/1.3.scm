(define (square x) (* x x))

(define (sum-of-squares x y)
  (display x)
  (display y)
  (+ (square x) (square y)))

(define (sum-square-largest-two x y z)
    (cond ((or (>= x y z) (>= y x z)) (sum-of-squares x y))
          ((or (>= x z y) (>= z x y)) (sum-of-squares x z))
          ((or (>= y z x) (>= z y x)) (sum-of-squares y z))
          )
    )

;(sum-square-largest-two 2 2 3)
