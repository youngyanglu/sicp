(define (fringe l)
  (cond ((null? l) '())
        ((not (pair? l)) (list l))
        (else (append (fringe (car l)) (fringe (cdr l))))
        )))

(define x 
  (list (list 1 2) (list 3 4)))

;(define x 
  ;(list (list 1 2)))

;(define x (list 1 2 3))

;(fringe x)
;(1 2 3 4)

;(fringe (list x x))
;(1 2 3 4 1 2 3 4)
