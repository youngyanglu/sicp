(define (adjoin-position new-row k rest-of-queens) (append rest-of-queens (list (cons new-row k))))

;(adjoin-position 2 3 (list (cons 0 0) (cons 1 2)))
;((cons 0 0) (cons 1 2) (cons 2 3))

(define empty-board '())

(define (last_element l)
  (cond ((null? (cdr l)) (car l))
        (else (last_element (cdr l)))))

(define (safe? board-size positions)
  (define (safe-one-position q1 q2)
   (let ((x1 (car q1))
        (y1 (cdr q1))
        (x2 (car q2))
        (y2 (cdr q2))
        )
    (NOT (OR (= x1 x2) (= y1 y2) (= (- x1 x2) (- y1 y2)))
         ))
  )

  (define new-queen (last_element positions))

  (accumulate (lambda (queen safe) 
    (cond ((null? queen) true)
          ((not safe) safe)
          ((AND (= (car queen) (car new-queen)) (= (cdr queen) (cdr new-queen))) safe)
        (else (safe-one-position queen new-queen)))
  ) true positions)
)

;(safe? 3 (list (cons 1 1) (cons 4 2)))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
     '() 
      (cons low 
            (enumerate-interval 
             (+ low 1) 
             high))))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) 
           (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position 
                    new-row 
                    k 
                    rest-of-queens))
                 (enumerate-interval 
                  1 
                  board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))


;(queens 8)
