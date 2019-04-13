(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx) (f x) (f + x dx))) 3)
    ))

(define (repeated f n)
(define (repeated-iter fm m)
  (if (= m 1) fm
    (repeated-iter (compose f fm) (- m 1))
    ))
  (lambda (x)
    ((repeated-iter f n) x)
  ))

(define (compose f g)
  (lambda (x) 
    (f ( g x))
    ))

(define dx 0.0001)

(define (n-fold-smoothed f n)
  ((repeated smooth n) f
  ))
