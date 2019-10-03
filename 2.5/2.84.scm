(put 'raise 'scheme-number
       (lambda (x) (make-rational x 1)))

(put 'raise 'rational
       (lambda (x) (make-real (/ (numer x) (denom x))))) ; some hypothetical real package that deals with arbitrary decimals

(put 'raise 'real
       (lambda (x) (make-complex-from-real-imag x 0)))

(define (raise x) (apply-generic 'raise x))

(define (level type)
  (cond ((eq? 'scheme-number type) 0)
        ((eq? 'rational type) 1)
        ((eq? 'real type) 2)
        ((eq? 'complex type) 3)
        ))

; takes in all type-tags and returns arg with highest type
; map through all args and return list all coerced to highest type

(define (find-highest type-tags highest-so-far)
  (if (null? type-tags) 
    highest-so-far
    (if (< highest-so-far (car type-tags))
           (find-highest (cdr type-tags) (car type-tags))
           (find-highest (cdr type-tags) highest-so-far)
        )
    ))

(define (coerce arg1 end-type)
    (if (= (type-tag arg1) end-type) 
      arg1
      (coerce (raise arg1) end-type)
  ))

(define (coerce-all args end-type)
  (if (null? args) '()
    (cons (coerce (car args) end-type) (coerce-all (cdr args) end-type))
    ))

(define (all-same-type type-tags)
  (cond ((null? type-tags) #t)
        (null? (cdr type-tags) #t)
        (else (and (eq? (car type-tags) (cadr type-tags)) (all-same-type (cdr type-tags))))
    ))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (all-same-type type-tags)
            (error 
               "No method for these types"
               (list op type-tags))
            (let ((highest-type (find-highest type-tags 0))
                (coerced-args (coerce-all args highest-type))
                )
            (apply-generic op args))
            )
                        (error 
               "No method for these types"
               (list op type-tags))))))
