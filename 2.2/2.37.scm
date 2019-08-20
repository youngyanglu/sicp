(define (matrix-*-vector m v)
  (map (lambda (row) 
   (list (* (car v) (car row)) (* (cadr v) (cadr row))
  )) m))


(define matrix (list (list 1 2) (list 3 4)))
(define vector (list 1 2))

;(matrix-*-vector matrix vector)

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
     '() 
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (transpose mat)
  (accumulate-n cons '() mat))

;(transpose matrix)

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row)
           (map (lambda (col) 
               (+ (* (car col) (car row)) (* (cadr col) (cadr row))
           ))
           cols)) m)))

;(matrix-*-matrix matrix matrix)
