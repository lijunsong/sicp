(load "machine-stack-module.scm")
(load "machine-register-module.scm")
(load "machine-machine-module.scm")
(load "machine-assemble-module.scm")

; (save <register-name>) needs to save contents as well as register
; name on the stack.

(define (make-register-info name contents)
  (cons name contents))
(define (get-register-info-name info)
  (car info))
(define (get-register-info-contents info)
  (cdr info))

(define (make-save inst machine stack pc)
  (let* ((name (stack-inst-reg-name inst))
         (reg (get-register machine name)))
    (lambda ()
      (let ((reg-info (make-register-info name (get-contents reg))))
        (push stack reg-info)
        (advance-pc pc)))))

(define (make-restore inst machine stack pc)
  (let* ((name (stack-inst-reg-name inst))
         (reg (get-register machine name)))
    (lambda ()
      (let* ((reg-info (pop stack))
             (name-on-stack (get-register-info-name reg-info)))
        (cond ((not (eq? name-on-stack name))
               (error 'make-restore
                      "Invalid restore argument-- STORE " name))
              (else
               (set-contents! reg (get-register-info-contents reg-info))
               (advance-pc pc)))))))


;; test b
(define test-controller
  '((assign continue (label fib-done))
 fib-loop
   (test (op <) (reg n) (const 2))
   (branch (label immediate-answer))
   ;; set up to compute Fib(n - 1)
   (save continue)
   (assign continue (label afterfib-n-1))
   (save n)                           ; save old value of n
   (assign n (op -) (reg n) (const 1)); clobber n to n - 1
   (goto (label fib-loop))            ; perform recursive call
 afterfib-n-1                         ; upon return, val contains Fib(n - 1)
   (restore n)
   (restore continue)
   ;; set up to compute Fib(n - 2)
   (assign n (op -) (reg n) (const 2))
   (save continue)
   (assign continue (label afterfib-n-2))
   (save val)                         ; save Fib(n - 1)
   (goto (label fib-loop))
 afterfib-n-2                         ; upon return, val contains Fib(n - 2)
   (restore n)                        ;; Invalid argument!
   (restore continue)
   (assign val                        ;  Fib(n - 1) +  Fib(n - 2)
           (op +) (reg val) (reg n))
   (goto (reg continue))              ; return to caller, answer is in val
 immediate-answer
   (assign val (reg n))               ; base case:  Fib(n) = n
   (goto (reg continue))
 fib-done))

(define test-machine
  (make-machine
   '(n continue val)
   (list (list '< <)
         (list '- -)
         (list '+ +))
   test-controller))

(set-register-contents! test-machine 'n 10)
(assert/exn (start test-machine) "Invalid")
