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

(define (fermat-test n a)
    (= (expmod a n n) a))

(define (fast-prime-iter n test)
  (define test-result (fermat-test n test))

  (cond ((= test 1) true)
        ((fermat-test n test) (fast-prime-iter n (- test 1)))
        (else false)))

(define (fast-prime? n)
  (fast-prime-iter n (- n 1)))

;(fast-prime? 21)
;(fast-prime? 31)
;(fast-prime? 561)
;(fast-prime? 1105)
;(fast-prime? 1729)
;(fast-prime? 2465)
;(fast-prime? 2821)
;(fast-prime? 6601)

