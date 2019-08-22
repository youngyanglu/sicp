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

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (unique-triples n) (flatmap 
       (lambda (k) 
        (flatmap (lambda (i) (map (lambda (j) 
               (list j i k))
             (enumerate-interval 1 (- i 1)))
      ) (enumerate-interval 1 (- k 1))))
       (enumerate-interval 1 n)))

(unique-triples 3)

(define (unique-triples-sum n s)   
  (define (sum? ls) 
    (= s (accumulate + 0 ls)))
    (filter sum? (unique-triples n))
  )

;(unique-triples-sum 4 8)
