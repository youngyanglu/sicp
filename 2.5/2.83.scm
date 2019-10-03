(put 'raise 'scheme-number
       (lambda (x) (make-rational x 1)))

(put 'raise 'rational
       (lambda (x) (make-real (/ (numer x) (denom x))))) ; some hypothetical real package that deals with arbitrary decimals

(put 'raise 'real
       (lambda (x) (make-complex-from-real-imag x 0)))

