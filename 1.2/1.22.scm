(define (smallest-divisor n)
  (find-divisor n 2))

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

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
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


;(search-prime-iter 10000000000 10000000061)
;(search-prime-iter 100000000000 100000000057)
