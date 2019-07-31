(define (fringe l)
  (cond ((null? l) '())
        ((not (pair? l)) l)
        (else (append (deep-reverse (cdr l)) (list (deep-reverse (car l))))
        )))

(define x 
  (list (list 1 2) (list 3 4)))

(fringe x)
(1 2 3 4)

(fringe (list x x))
(1 2 3 4 1 2 3 4)
