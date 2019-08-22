(define (equal? l1 l2) 
  (cond 
      ((AND (NOT (pair? l1)) (NOT (pair? l2))) 
         (eq? l1 l2))
      ((AND (pair? l1) (pair? l2)) 
         (AND (equal? (car l1) (car l2)) 
             (equal? (cdr l1) (cdr l2))
             ))
        (else #f)))


(equal? '(this is a list) 
        '(this is a list))

(equal? '(this is a list) 
        '(this (is a) list))


