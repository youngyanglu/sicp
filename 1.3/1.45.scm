(define (fixed-point f first-guess)
  (display "4")
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define tolerance 0.000001)

(define (close-enough? x y) 
  (< (abs (- x y)) 0.001))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  (fixed-point 
   (lambda (y) (average y (/ x y)))
   1.0))

(define (cube-root x)
  (fixed-point 
   (lambda (y) (average y (/ x (* y y))))
   1.0))

(define (repeated f n)
  (display "3") 
(define (repeated-iter fm m)
  (display "5") 
  (if (= m 1) fm
    (repeated-iter (compose f fm) (- m 1))
    ))
(display "6") 
  (lambda (x)
    ((repeated-iter f n) x)
  ))

(define (compose f g)
  (lambda (x) 
    (f ( g x))
    ))

(define (fourth-root x)
    (define (average-damp y)
        (average y (/ x (* y y y))))

  (fixed-point 
  (repeated average-damp 2)
   1.0))

  
(define (fifth-root x)
    (define (average-damp y)
        (average y (/ x (* y y y y))))

  (fixed-point 
  (repeated average-damp 2)
   1.0))

 (define (get-max-pow n) 
   (define (iter p r) 
     (if (< (- n r) 0) 
       (- p 1) 
       (iter (+ p 1) (* r 2)))) 

   (iter 1 2)) 

(define (pow b p)
  (define (pow-iter b total p)
    (cond ((= p 0) 1)
          ((= p 1) b)
          (else (pow-iter b (* total p) (- p 1)))
          )))

(define (nth-root x n)
  (display "1")
    (define (average-damp y)
        (average y (/ x (pow y (- n 1)))))

 (display "2")
  (fixed-point 
  (repeated average-damp (get-max-pow n))
   1.0))

;(cube-root 27)
;(fourth-root 81)
(nth-root 32 5)
;(get-max-pow 5)


