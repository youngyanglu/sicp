(put 'project 'rational 
     (lambda (x) (make-scheme-number (round (/ (numer x) (denom x))))))

(put 'project 'real 
     (lambda (x) (make-rational (round x) 1))) ; technically this projection only works for real numbers that are actually ints, won't work for rational decimals

(put 'project 'complex 
     (lambda (x) (make-real (real-part x))))

(put 'raise 'scheme-number
     (lambda (x) (make-rational x 1)))

(put 'raise 'rational
     (lambda (x) (make-real (/ (numer x) (denom x))))) ; some hypothetical real package that deals with arbitrary decimals

(put 'raise 'real
     (lambda (x) (make-complex-from-real-imag x 0)))

(define (drop val)
  (equ? (raise (project val)) val))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (drop (apply proc (map contents args)))
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

