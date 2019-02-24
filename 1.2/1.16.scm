(define (exponential b n)
  (expo-iter 1 b n))

(define (expo-iter extras total left)
  (cond ((= left 1)
        (* extras total))
        ((even? left)
         (expo-iter extras (square total) (/ left 2)))
        (else
         (expo-iter (* total extras) total (- left 1)))
        ))


(define (square n)
  (* n n))

(define (even? n)
  (= (remainder n 2) 0))

;(exponential 2 10)
