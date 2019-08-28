(define (get-record personnel-file employee-name)
    ((get 'get-record (type-tag personnel-file)) 
     employee-name
     (contents personnel-file)
     ))


(define (get-value key employee-record)
  ((get 'get-value (type-tag employee-record))
   key 
   (contents employee-record)
  ))

(define (get-employee-name record)
  (get-value 'name record))

(define (find-employee-record employee-name all-division-files)
    (cond 
      ((null? all-division-files) 
         #f)
      ((not (null? (get-record (car all-division-files) name))) (get-record (car all-division-files) name)) 
        (else (find-employee-record employee-name (cdr all-division-files))))
  
