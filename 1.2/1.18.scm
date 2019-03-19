(define (multiply b n)
  (mult-iter 0 b n))

(define (mult-iter extras total left)
  (cond ((= left 1)
        (+ extras total))
        ((= left 0)
        0)
        ((even? left)
         (mult-iter extras (double total) (/ left 2)))
        (else
         (mult-iter (+ total extras) total (- left 1)))
        ))


(define (double n)
  (+ n n))

(define (even? n)
  (= (remainder n 2) 0))

;(multiply 11 0)

