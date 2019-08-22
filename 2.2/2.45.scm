(define right-split (split beside below))
(define up-split (split below beside))

(define (split proc1 proc2)
  (define (split-proc painter n)
    (if (= n 0)
      painter
      (let ((smaller (split-proc painter 
                                  (- n 1))))
        (proc1 painter 
                (proc2 smaller smaller)))))
  split-proc
  )
