(put 'equ? '(scheme-number scheme-number)
       =)

(define (complex-equal z1 z2)
  (and (equ? (real-part z1) (real-part z2)) (equ? (imag-part z1) (imag-part z2))))

(put 'equ? '(complex complex)
       complex-equal)

(define (rat-equal r1 r2)
  (and (equ? (numer r1) (numer r2)) (equ? (denom r1) (denom r2))))

(put 'equ? '(rational rational)
       rat-equal)

