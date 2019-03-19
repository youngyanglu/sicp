(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b n)
  (define (argument k) 
    (+ a (* k (/ (- b a) n))))

(define (term k)
  (cond ((or(= k 0) (= k n)) (f (argument k)))
        ((even? k) (*(f (argument k)) 2))
        (else (* (f (argument k)) 4))))

(define (next x) 
  (+ x 1))

  (* (sum term 0 next n) 
     (/ (- b a) (* n 3))))

(define (cube x) (* x x x))

;(integral cube 0 1 100)
;(integral cube 0 1 1000)
