(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) 
           (if (same-variable? exp var) 
               1 
               0))
         (else ((get 'deriv (operator exp)) 
                (operands exp) 
                var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))

(define (deriv-prod exp var)
  (make-sum
    (make-product 
      (multiplier exp)
      (deriv (multiplicand exp) var))
    (make-product 
      (deriv (multiplier exp) var)
      (multiplicand exp))))

(define (deriv-exp exp var)
  (make-product 
    (make-product (exponent exp) (make-exponentiation 
      (base exp) (make-sum (exponent exp) (- 0 1)))) 
        (deriv (base exp) var))
)

(define (install-deriv-package)
  ;; interface to the rest of the system
  (put 'deriv '(+) deriv-sum)
  (put 'deriv '(*) deriv-prod)
  (put 'deriv '(**) deriv-exp)
  'done)

(define (install-deriv-package-reverse)
  ;; interface to the rest of the system
  (put '+ 'deriv deriv-sum)
  (put '* 'deriv deriv-prod)
  (put '** 'deriv deriv-exp)
  'done)
