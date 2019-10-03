(define random-init 0)

 (define (rand-update x) (+ x 1)) ; for test

 (define rand 
   (let ((x random-init)) 
     (define (dispatch message) 
       (cond ((eq? message 'generate) 
               (begin (set! x (rand-update x)) 
                      x)) 
             ((eq? message 'reset) 
               (lambda (new-value) (set! x new-value))))) 
     dispatch)) 
; !! notice definition of rand is very much as a variable not a procedure (even
; though rand is set to be equal to a procedure, dispatch), this tripped me
; up for a while
; define dispatch inside let so that x state is maintained through subsequent
; rand calls
; rand is equivalent to dispatch except dispatch has access to x

(rand 'generate) 
(rand 'generate) 
((rand 'reset) 0) 
(rand 'generate) 
