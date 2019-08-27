(define (generate-huffman-tree pairs)
  (successive-merge 
   (make-leaf-set pairs)))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set 
         (make-leaf (car pair)    ; symbol
                    (cadr pair))  ; frequency
         (make-leaf-set (cdr pairs))))))

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) 
         (cons x set))
        (else 
         (cons (car set)
               (adjoin-set x (cdr set))))))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) 
                (symbols right))
        (+ (weight left) (weight right))))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (successive-merge ordered-pairs)
  (if (null? (cdr ordered-pairs))
    (car ordered-pairs)
     (successive-merge (append (list (make-code-tree (car ordered-pairs) (cadr ordered-pairs))) (cddr ordered-pairs)))
    )
)

(generate-huffman-tree (list (list 'A 2) (list 'BOOM 1) (list 'GET 2) (list 'JOB 2) (list 'NA 16)))
