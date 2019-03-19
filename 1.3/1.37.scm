(define (cont-frac n d k)
  (define (cont-frac-iter total-so-far k)
    (if (= k 0) total-so-far
      (cont-frac-iter (/ (n k) (+ (d k) total-so-far)) (- k 1))
      ))
  (cont-frac-iter 0 k)
  )

(define (cont-frac n d k)
  (define (cont-frac-rec countdown)
    (if (= countdown k) (/ (n k) (d k))
      (/ (n countdown) (+ (d countdown) (cont-frac-rec (+ countdown 1))
      ))))
  (cont-frac-rec 0)
  )


;(cont-frac (lambda (i) 1.0)
           ;(lambda (i) 1.0)
           ;13)
