(define (repeated f n)
(define (repeated-iter fm m)
  (if (= m 1) fm
    (repeated-iter (compose f fm) (- m 1))
    ))
  (lambda (x)
    ((repeated-iter f n) x)
  ))

(define (compose f g)
  (lambda (x) 
    (f ( g x))
    ))

;((repeated square 2) 5)

