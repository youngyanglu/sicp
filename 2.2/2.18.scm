(define (last-pair l)
  (cond ((null? l) "nil")
        ((null? (cdr l)) (car l))
        (else (last-pair (cdr l)))
    ))

(define (reverse l)
  (cond ((null? l) "nil")
        ((null? (cdr l)) l)
        (else (append (reverse (cdr l)) (list (car l))))
        ))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) 
            (append (cdr list1) 
                    list2))))

;(append (list 23 72) (list 149 34))
;(reverse (list 23 72 149 34))
