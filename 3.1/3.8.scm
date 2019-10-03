(define f 
  (let ((last -1))
    (define (dispatch num)
     (cond ((and (= num 1) (= last -1)) (begin (set! last num) 1))
           (else (begin (set! last num) 0))
       ))
    dispatch
    ))

(+ (f 1) (f 0))
