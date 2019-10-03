(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(count-pairs (cons (cons 'a 'b) (cons 'c 'd)))

(define a (cons 'a '()))
(define b (cons a '()))
(define c (cons a b))
(count-pairs c)

(define x (cons 'a '()))
(define y (cons x x))
(define z (cons y y))

(count-pairs z)

(define i '(a b c))
(set-cdr! (cddr i) i)
(count-pairs i)
