(define (count-pairs-new x)
  ;(define (seen? seenpairs pair) #f)
  (define (seen? seenpairs pair) 
    (cond ((not (pair? seenpairs)) #f)
          ((eq? (car seenpairs) pair) #t)
          (else (seen? (cdr seenpairs) pair))))

  (let ((seen '()))
    (define (count-pairs x)
      (cond ((not (pair? x)) 0)
            ((seen? seen x) 0)
            (else (begin (set! seen (cons x seen))
                         (+ (count-pairs (car x))
                            (count-pairs (cdr x))
                            1)))
            ))
    
    (count-pairs x)
    ))

;(count-pairs-new (cons (cons 'a 'b) (cons 'c 'd)))

;(define a (cons 'a '()))
;(define b (cons a '()))
;(define c (cons a b))
;(count-pairs-new c)


;(define x (cons 'a '()))
;(define y (cons x x))
;(define z (cons y y))

;(count-pairs-new z)

;(define i '(a b c))
;(set-cdr! (cddr i) i)
;(count-pairs-new i)
