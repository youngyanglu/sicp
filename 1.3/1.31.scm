;(define (product term a next b)
  ;(if (> a b)
    ;1
    ;(* (term a) 
       ;(product term (next a) next b))
    ;))

(define (product term a next b)
(define (product-iter a total) 
  (if (> a b)
    total
    (product-iter (next a) (* total (term a)))))
    (product-iter a 1)
    ))


(define (inc n) (+ n 1))
(define (identity x) x)

(define (factorial n)
  (product identity 1 inc n))

;(factorial 5)

(define (term x) 
  (if (even? x)
    (/ x (+ x 1))
    (/ (+ x 1) x)))

(define (pi n) 
  (* (product term 2 inc n) 4))

(pi 90)
