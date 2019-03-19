(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 
       tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))


(define (f x)
  (/ (log 1000) (log x)))

(define (f-damping x)
  (/ ( + x (/ (log 1000) (log x))) 2))

;(fixed-point f 2)
;(fixed-point f-damping 2)
