(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) 
                (- n 1))))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) 
            (append (cdr list1) 
                    list2))))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (same-parity . w) 
  (define (same-parity-iter w index match)
    (if (< index 0)
      match
    (if (= (remainder (list-ref w 0) 2) (remainder (list-ref w index) 2))
      (same-parity-iter w (- index 1) (cons (list-ref w index) match )
                        )
      (same-parity-iter w (- index 1) match)
      )
      )
    )

  (define match (list))
  (same-parity-iter w (- (length w) 1) match)
  )

(same-parity 2 3 4 5 6 7)
