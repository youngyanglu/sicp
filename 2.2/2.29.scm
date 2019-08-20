;(define (make-mobile left right)
  ;(list left right))

;(define (left-branch mobile)
  ;(car mobile))

;(define (right-branch mobile)
  ;(cadr mobile))

;(define (make-branch length structure)
  ;(list length structure))

;(define (branch-length branch)
  ;(car branch))

;(define (branch-structure branch)
  ;(cadr branch))


(define (make-mobile left right)
  (cons left right))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))

(define (make-branch length structure)
  (cons length structure))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cdr branch))

(define (total-weight mobile)
    (cond ((null? mobile) 0)
          ((not (pair? mobile)) mobile)
          (else (+ (total-weight (branch-structure (left-branch mobile))) (total-weight (branch-structure (right-branch mobile)))))
          )
  )

(define (balanced mobile)
  (define (locally-balanced mobile) 
     (let ((left (left-branch mobile))
         (right (right-branch mobile)))
         (= (* (branch-length left) (total-weight (branch-structure left))) (* (branch-length right) (total-weight (branch-structure right))))
      ))
    (cond ((null? mobile) true)
          ((not (pair? mobile)) true)
          (else (and (locally-balanced mobile) (balanced (branch-structure (left-branch mobile))) (balanced (branch-structure (right-branch mobile)))))
          )
    )



(define A (make-branch 5 4))
(define C (make-branch 2 2))
(define D (make-branch 2 2))
(define baby-mobile (make-mobile C D))
(define B (make-branch 5 baby-mobile))
(define mobile (make-mobile A B))

(balanced mobile)
