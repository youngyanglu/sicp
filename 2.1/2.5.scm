(define (cons a b)
    (* (expt 2 a) (expt 3 b))
  )

(define (car z) 
  (if (= (remainder z 2) 0) (+ 1 (car (/ z 2))) 
   0 
    ))


(define (cdr z) 
  (if (= (remainder z 3) 0) (+ 1 (car (/ z 3))) 
   0 
    ))

(define x (cons 1 2))
(display x)
(display (car x))
(display (cdr x))
