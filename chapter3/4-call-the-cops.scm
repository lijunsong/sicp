(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (password-error dummy) "Incorrect password")
  (define (call-the-cops dummy) "Call the Cops!")
  (define dispatch
    (let ((incorrect-pwd 1))
      (lambda (pwd m)
        (if (not (eq? pwd password))
            (begin
              (if (>= incorrect-pwd 7)
                  call-the-cops
                  (begin (set! incorrect-pwd (+ 1 incorrect-pwd))
                         password-error)))
            (begin
              (set! incorrect-pwd 1)
              (cond ((eq? m 'withdraw) withdraw)
                    ((eq? m 'deposit) deposit)
                    (else (error "Unknown request -- MAKE-ACCOUNT"
                                 m))))))))
  dispatch)

(define acc (make-account 100 'secret-password))
