(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (OR (AND (> n 0) (< d 0)) (AND (< n 0) (> d 0))) (cons (/ (- 0 n) g) (/ (- 0 d) g))
      (cons (/ n g) (/ d g))
      )
    )
  )

(define (numer x) (car x))
(define (denom x) (cdr x))
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))


;(define one-half (make-rat -1 2))
;(print-rat one-half)

(print "jello")
