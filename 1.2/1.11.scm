(define (frecursive n)
  (if (> 3 n)
    n
    (+ (frecursive (- n 1)) (* 2 (frecursive (- n 2))) (* 3 (frecursive (- n 3))))
    ))

;(frecursive 5)

(define (fiterative n)
  (f-iter 2 1 0 n))

(define (f-iter x y z n)
  (if (= n 0)
    z
    (f-iter (+ x (* 2 y) (* 3 z)) x y (- n 1))
    ))

;(fiterative 6)
