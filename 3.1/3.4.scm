(define (make-account balance password)
  (let ((incorrectpasscount 0))
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance 
               (- balance amount))
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (call-the-cops) "Calling Cops")
  (define (incorrect-pass amount)
    (set! incorrectpasscount (+ incorrectpasscount 1))
    (if (< 7 incorrectpasscount)
      (call-the-cops)
      "Incorrect password"
      ))
  (define (dispatch pw m)
    (cond ((not (eq? pw password)) incorrect-pass)
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request: 
                       MAKE-ACCOUNT" m))))
    dispatch
    ))

(define acc 
  (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40)

((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
