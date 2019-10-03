(define i '(a b c))
(set-cdr! (cddr i) i)

(define (cycle? list)
  (define (seen? seenpairs pair) 
    (cond ((not (pair? seenpairs)) #f)
          ((eq? (car seenpairs) pair) #t)
          (else (seen? (cdr seenpairs) pair))))

  (let ((seen '()))
    (define (cycle-helper x)
      (cond ((not (pair? x)) #f)
            ((seen? seen x) #t)
            (else (begin (set! seen (cons x seen))
                         (or (cycle-helper (car x))
                            (cycle-helper (cdr x))
                            )))
            ))
    
    (cycle-helper list)
    ))

(cycle? i)

