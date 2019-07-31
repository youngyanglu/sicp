;find fixed point of function by repeatedly applying guess to function
(define (fixed-point f first-guess)
  (define tolerance 0.000001)

  (define (close-enough? a b)
    (< (abs (- a b)) 
       tolerance))

  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;returns function that applies f repeatedly to x n times
(define (repeated f n)
  (if (= n 1) f
    (lambda (x) (f ((repeated f (- n 1)) x)))))
  
;formula for calculating number of times average-damp function should be
;applied 
(define (get-max-pow n) 
  (define (iter p r) 
    (if (< (- n r) 0) 
      (- p 1) 
      (iter (+ p 1) (* r 2)))) 

  (iter 1 2)) 

;b^p
(define (pow b p)
  (define (pow-iter base total power)
    (cond ((= power 0) total)
          (else (pow-iter base (* total base) (- power 1)))
          ))
  (pow-iter b 1 p))

(define (average x y)
  (/ (+ x y) 2.0))

;returns function that takes input y and finds average of y and f(y)
(define (average-damp f)
   (lambda (y) (average y (f y))))


;(repeated average-damp (get-max-pow n)) --> return fun that applies
;average-damp (get-max-pow n) times to an argument (in this case, that argument is
;the lambda x / y^(n - 1))

;nb: average-damp takes a function as argument, therefore so does a repeated
;average-damp

 (define (nth-root n x) 
   (fixed-point ((repeated average-damp (get-max-pow n)) 
                 (lambda (y) (/ x (pow y (- n 1))))) 
                1.0)) 

;nth-root can be seen to simplify to sqrt function when n = 2
(define (sqrt x)
  (fixed-point 
   (lambda (y) (average y (/ x y)))
   1.0))
   
;nth-root can be seen to simplify to cube-root function when n = 3
(define (cube-root x)
  (fixed-point 
   (lambda (y) (average y (/ x (* y y))))
   1.0))

;example
(nth-root 5 32)
