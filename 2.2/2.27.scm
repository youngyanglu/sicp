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

(define (deep-reverse l)
  (cond ((null? l) '())
        ((not (pair? l)) l)
        (else (append (deep-reverse (cdr l)) (list (deep-reverse (car l))))
        )))

(define x 
  (list (list 1 2) (list 3 4)))

x
;((1 2) (3 4))

(reverse x)
;((3 4) (1 2))

(deep-reverse x)
;((4 3) (2 1))

