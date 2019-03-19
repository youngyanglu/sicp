(define (accumulate combiner filter null-value term a next b)
            (define (f term a next b)
            (cond ((> a b) null-value)
                  ((filter a) (combiner (term a)
                        (f term (next a) next b)))
                  (else (f term (next a) next b))
              ))
            (f term a next b))

;(define (accumulate combiner null-value term a next b)
            ;(define (f-iter a total)
            ;(if (> a b)
             ;total 
              ;(f-iter (next a) (combiner total (term a)))))
            ;(f-iter a null-value))

(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (add-one x) (+ x 1))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) 
         n)
        ((divides? test-divisor n) 
         test-divisor)
        (else (find-divisor 
               n 
               (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (sum-of-squares-prime a b)
  (accumulate + prime? 0 square a add-one b))

;(sum-of-squares-prime 1 10)

(accumulate + relatively-prime? 1 add-one 10)
