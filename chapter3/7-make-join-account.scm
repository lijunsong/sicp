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

  ;; joint procedure to clone the account with new password
  (define (joint new-password)
    (define (dispatch pwd m)
      (cond ((not (eq? pwd new-password)) password-error)
            ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'joint) joint)
            ((eq? m 'print-balance) print-balance)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch)

  ;; new dispatch end

  (define (dispatch pwd m)
    (cond ((not (eq? pwd password)) password-error)
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          ((eq? m 'joint) joint)
          ((eq? m 'print-balance) print-balance)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))

  dispatch)

;; new make-account

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


  (define (joint new-password)
    (receiver new-password))

  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          ((eq? m 'joint) joint)
          ((eq? m 'print-balance) print-balance)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))

  (define (receiver pwd)
    (lambda (input-pwd m)
      (cond ((not (eq? input-pwd pwd)) password-error)
            (else
             (dispatch m)))))

  (receiver password))

(define (make-joint account its-password new-password)
  ((account its-password 'joint) new-password))


;;; tests begin

;; regression test
(load "../testframe.scm")

(let ((acc (make-account 100 'pwd)))
  (assert= ((acc 'pwd 'deposit) 100) 200)
  (assert= ((acc 'pwd 'withdraw) 200) 0)
  (assertequal? ((acc 'pwd 'withdraw) 100) "Insufficient funds")
  (assertequal? ((acc 'pwd1 'deposit) 100) "Incorrect password")
  (assert= ((acc 'pwd 'deposit) 100) 100))

;; new test

(let* ((peter-acc (make-account 1000 'peter))
       (paul-acc  (make-joint peter-acc 'peter 'paul)))
  (assert= ((peter-acc 'peter 'deposit) 100) 1100)
  (assert= ((paul-acc 'paul 'deposit) 100) 1200)
  (assert= ((peter-acc 'peter 'deposit) 100) 1300)  
  ; deposit testing end

  ; paul can withdraw money from peter's account
  (assert= ((paul-acc 'paul 'withdraw) 1300) 0)
  (assertequal? ((peter-acc 'peter 'withdraw) 100) "Insufficient funds")

  ; peter can either
  (assert= ((paul-acc 'paul 'deposit) 100) 100)
  (assert= ((peter-acc 'peter 'withdraw) 100) 0)
  (assertequal? ((paul-acc 'paul 'withdraw) 100) "Insufficient funds")

  ; password
  (assertequal? ((paul-acc 'peter 'deposit) 100) "Incorrect password")
  (assertequal? ((peter-acc 'paul 'deposit) 100) "Incorrect password")
  
  (assertequal? ((paul-acc 'anything 'deposit) 100) "Incorrect password")
  (assertequal? ((peter-acc 'anything 'deposit) 100) "Incorrect password"))

