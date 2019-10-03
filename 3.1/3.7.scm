(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance 
                     (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (incorrect-pass amount)
    "Incorrect password"
    )
  (define (dispatch pw m)
    (cond ((eq? m 'check-password) (eq? pw password))
          ((not (eq? pw password)) incorrect-pass)
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request: 
                 MAKE-ACCOUNT" m))))
  dispatch)

(define (make-joint acc password new-password)
  (define (incorrect-pass amount)
    "Incorrect password"
    )
  (define (dispatch pw m)
    (cond ((not (eq? pw new-password)) incorrect-pass)
          ((eq? m 'withdraw) (acc password 'withdraw))
          ((eq? m 'deposit) (acc password 'deposit))
          (else (error "Unknown request: 
                 MAKE-ACCOUNT" m))))
  (if (acc password 'check-password)
    dispatch 
   (display "Incorrect password") 
    ))

(define paul-acc 
  (make-account 100 'secret-password))

((paul-acc 'secret-password 'withdraw) 40)

((paul-acc 'some-other-password 'deposit) 50)

(paul-acc 'some-other-password 'check-password)

(define peter-acc
  (make-joint paul-acc 
              'secret-password
              'rosebud))

((peter-acc 'rosebud 'withdraw) 40)

((peter-acc 'some-other-password 'deposit) 50)
