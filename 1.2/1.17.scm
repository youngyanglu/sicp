(define (fast-mult m n)
  (cond ((= n 0) 
         0)
        ((= n 1) 
         m)
        ((even? n) 
         (double (fast-mult m (/ n 2))))
        (else 
         (+ m (fast-mult m (- n 1))))))

;(fast-mult 0 11)
