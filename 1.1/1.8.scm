(define (square x) (* x x))

(define (abs x)
  (if (< x 0)
    (- 0 x)
    x))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (good-enough? guess previous-guess)
  (< (abs (- guess previous-guess)) (* 0.001 guess)))

(define (cube-iter guess previous-guess x)
  (if (good-enough? guess previous-guess)
    guess
    (cube-iter (improve guess x)
               guess
               x)))

(define (cube x)
  (cube-iter 1.0 2.0 x))

;(cube 10000)
