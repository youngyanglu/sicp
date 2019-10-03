(define (make-monitored f)
  (let ((callcount 0))

  (lambda (arg)
    (cond ((eq? arg 'how-many-calls?) callcount)
          ((eq? arg 'reset-count) (set! callcount 0))
          (else 
            (begin (set! callcount (+ callcount 1))
             (f arg))
            ))
    )))

(define s (make-monitored sqrt))

(s 100)

(s 'how-many-calls?)
(s 'reset-count)
(s 'how-many-calls?)
