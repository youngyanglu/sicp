(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (append (list x) set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

;(adjoin-set 3 (list 1 2 4 5))
