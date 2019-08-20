(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (rest-item) 
                            (append rest-item (list (car s)
                            ))) rest)))))

;(subsets (list 1 2 3 4))
