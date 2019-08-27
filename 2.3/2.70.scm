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

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

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
     (successive-merge (adjoin-set (make-code-tree (cadr ordered-pairs) (car ordered-pairs)) (cddr ordered-pairs)))
    )
)

(define (encode message tree)
  (if (null? message)
      '()
      (append 
       (encode-symbol (car message) 
                      tree)
       (encode (cdr message) tree))))

(define (encode-symbol symbol tree) 
    (if (leaf? tree) 
      (if (equal? symbol (symbol-leaf tree))
        '()
        #f
        )
      (cond ((encode-symbol symbol (left-branch tree)) (cons 0 (encode-symbol symbol (left-branch tree))))
            ((encode-symbol symbol (right-branch tree)) (cons 1 (encode-symbol symbol (right-branch tree))))
            (else #f)
            )
      )
  )
(define lyrics '(GET A JOB 
                   SHA NA NA NA NA NA NA NA NA 
                   GET A JOB SHA NA NA NA NA NA NA NA NA 
                   WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP 
                   SHA BOOM))

(define tree (generate-huffman-tree (list (list 'A 2) (list 'BOOM 1) (list 'GET 2) (list 'JOB 2) (list 'NA 16) (list 'SHA 3) (list 'YIP 9) (list 'WAH 1))))

;(display tree)
(length (encode lyrics tree))
