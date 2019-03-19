(define (accumulate combiner null-value term a next b)
            (define (f term a next b)
            (if (> a b)
              null-value
              (combiner (term a)
                        (f term (next a) next b))
              ))
            (f term a next b))

(define (accumulate combiner null-value term a next b)
            (define (f-iter a total)
            (if (> a b)
             total 
              (f-iter (next a) (combiner total (term a)))))
            (f-iter a null-value))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (inc n) (+ n 1))
(define (identity x) x)

(define (factorial n)
  (product identity 1 inc n))

(define (sum-integers a b)
  (sum identity a inc b))

;(factorial 5)
;(sum-integers 1 10)
