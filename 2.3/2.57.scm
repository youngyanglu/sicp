(define (deriv exp var)
  (display "\n")
  (display exp)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (list (deriv (addend exp) var)
                   (deriv (augend exp) var))))
        ((product? exp)
         (make-sum (list 
           (make-product (list 
            (multiplier exp)
            (deriv (multiplicand exp) var)               
                           ))
          (make-product (list 
            (deriv (multiplier exp) var)
           (multiplicand exp))
           ))
          ))
        (else (error "unknown expression 
                      type: DERIV" exp))))


(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (numberlist? numlist)
  (if (null? numlist)
     #t 
      (AND (number? (car numlist)) (numberlist? (cdr numlist))))
  )

(define (=anynumberlist? exp num)
  (if (null? exp)
     #f
      (OR (=number? (car exp) num) (=anynumberlist? (cdr exp) num)))
  )

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (make-sum alist)
  (define alf (filter (lambda (x) (OR (NOT (number? x)) (NOT (= x 0)))) alist))
  (cond 
    ((numberlist? alf) (accumulate + 0 alf))
    ((=number? (car alf) 0) (make-sum (cdr alf)))
    ((null? (cdr alf)) (car alf))
    (else (append '(+) alf))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) 
  (if (null? (cdddr s))
    (caddr s)
    (append '(+) (cddr s)))
  )

(define (make-product mlist)
  (define mlf (filter (lambda (x) (OR (NOT (number? x)) (NOT (= x 1)))) mlist))
  (cond 
    ((=anynumberlist? mlf 0) 0)
    ((numberlist? mlf) (accumulate * 1 mlf))
    ((null? (cdr mlf)) (car mlf))
    (else (append '(*) mlf))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) 
  (if (null? (cdddr p))
    (caddr p)
    (append '(*) (cddr p)))
  )

(deriv '(* x y (+ x 3)) 'x)
