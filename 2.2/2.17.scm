(define (last-pair l)
  (display l)
  (cond ((null? l) "nil")
        ((null? (cdr l)) (car l))
        (else (last-pair (cdr l)))
    ))

(last-pair (list 23 72 149 34))
