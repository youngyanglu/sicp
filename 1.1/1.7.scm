(define (square x) (* x x))

(define (abs x)
  (if (< x 0)
    (- 0 x)
    x))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess previous-guess)
  (< (abs (- guess previous-guess)) (* 0.001 guess)))

(define (sqrt-iter guess previous-guess x)
  (if (good-enough? guess previous-guess)
    guess
    (sqrt-iter (improve guess x)
               guess
               x)))

(define (sqrt x)
  (sqrt-iter 1.0 2.0 x))

;(sqrt 1000000000)
