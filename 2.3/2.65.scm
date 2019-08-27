(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (list->tree elements)
  (car (partial-tree 
        elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size 
             (quotient (- n 1) 2)))
        (let ((left-result 
               (partial-tree 
                elts left-size)))
          (let ((left-tree 
                 (car left-result))
                (non-left-elts 
                 (cdr left-result))
                (right-size 
                 (- n (+ left-size 1))))
            (let ((this-entry 
                   (car non-left-elts))
                  (right-result 
                   (partial-tree 
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree 
                     (car right-result))
                    (remaining-elts 
                     (cdr right-result)))
                (cons (make-tree this-entry 
                                 left-tree 
                                 right-tree)
                      remaining-elts))))))))

(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list 
         (left-branch tree)
         (cons (entry tree)
               (copy-to-list 
                (right-branch tree)
                result-list)))))
  (copy-to-list tree '()))


;(tree->list (list->tree '(1 2 3 4 5 6 7 8 9 10 11)))

(define (union-set t1 t2)
  (define l1 (tree->list t1))
  (define l2 (tree->list t2))

  (define (merge-sort l1 l2) 
      (cond 
        ((null? l1) l2)
        ((null? l2) l1)
        ((= (car l1) (car l2)) (cons (car l1) (merge-sort (cdr l1) (cdr l2))))
        ((< (car l1) (car l2)) (cons (car l1) (merge-sort (cdr l1) l2)))
        (else (cons (car l2) (merge-sort l1 (cadr l2)))))
  )
  (merge-sort l1 l2)
  )

(union-set (list->tree '(1 2)) (list->tree '(2 3)))

(define (intersection-set t1 t2)
  (define l1 (tree->list t1))
  (define l2 (tree->list t2))

  (define (merge-sort l1 l2) 
      (cond 
        ((null? l1) '())
        ((null? l2) '())
        ((= (car l1) (car l2)) (cons (car l1) (merge-sort (cdr l1) (cdr l2))))
        ((< (car l1) (car l2)) (merge-sort (cdr l1) l2))
        (else (merge-sort l1 (cadr l2))))
  )
  (merge-sort l1 l2)
  )

(intersection-set (list->tree '(1 2)) (list->tree '(2 3)))

