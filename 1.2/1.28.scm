(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder 
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder 
          (* base (expmod base (- exp 1) m))
          m))))

(define (square x) (* x x))

(define (square-with-check base square-root n)
  (define (squared (square square-root)))
  (if (and (not (= base 0))
           (not (= base n - 1))
           (= (remainder (squared) n) 1)
           )
    0
    squared
    ))

(define (expmod-miller-rabin base exp n)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder 
           (square-with-check base (expmod-miller-rabin base (/ exp 2) n) n)
          n))
        (else
         (remainder 
          (* base (expmod-miller-rabin base (- exp 1) n))
          n))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod-miller-rabin a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) 
         (fast-prime? n (- times 1)))
        (else false)))
