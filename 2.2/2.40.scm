(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
     '() 
      (cons low 
            (enumerate-interval 
             (+ low 1) 
             high))))

(define (unique-pairs n) (accumulate 
 append
 '() 
 (map (lambda (i)
        (map (lambda (j) 
               (list j i))
             (enumerate-interval 1 (- i 1))))
      (enumerate-interval 1 n))))

;(unique-pairs 3)

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) 
        (cadr pair) 
        (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter 
        prime-sum?
(unique-pairs n))))

(prime-sum-pairs 3)

