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

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) 
         (fast-prime? n (- times 1)))
        (else false)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (fast-prime? n 10)
      (report-prime (- (runtime) 
                       start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-prime-iter first last)
  (define (search-iter curr last)
    (if (<= curr last) (timed-prime-test curr))
    (if (<= curr last) (search-iter (+ curr 2) last)))
   (search-iter (if (even? first ) (+ first 1) first) (if (even? last) (- last 1) last))
  )


;(search-prime-iter 10000000000000 10000000061000)
;(search-prime-iter 100000000000000 100000000057000)

