(load "machine-register-module.scm")
(load "machine-machine-module.scm")
(load "machine-assemble-module.scm")
(load "machine-new-stack-module.scm")

(define (make-new-machine)
  (let ((stack (make-stack))
        (flag (make-register))
        (pc (make-register))
        (the-instruction-sequence '()))
    (let ((regs (list (cons 'pc pc)
                      (cons 'flag flag))) ; assoc of name->register-object
          (the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics))))))
      (define (allocate-register name)
        (if (assoc name regs)
            (error 'machine "register exists: " name)
            (set! regs (cons (cons name (make-register)) regs)))
        'register-allocated)
      (define (lookup-register name)
        (let ((r (assoc name regs)))
          (if r
              (cdr r)
              (error 'machine "Unknown register: " name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc (car insts)))
                (execute)))))
      (define (dispatch msg)
        (cond ((eq? msg 'allocate-register) allocate-register)
              ((eq? msg 'lookup-register) lookup-register)
              ((eq? msg 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? msg 'stack) stack)
              ((eq? msg 'operations) the-ops)
              ((eq? msg 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? msg 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              (else
               (error 'machine "Unknown message: " msg))))
      dispatch)))

;; tests begin
(load "../testframe.scm")
(define controller
  '((assign continue (label fact-done))     ; set up final return address
   fact-loop
    (test (op =) (reg n) (const 1))
    (branch (label base-case))
    ;; Set up for the recursive call by saving n and continue.
    ;; Set up continue so that the computation will continue
    ;; at after-fact when the subroutine returns.
    (save continue)
    (save n)
    (assign n (op -) (reg n) (const 1))
    (assign continue (label after-fact))
    (goto (label fact-loop))
   after-fact
    (restore n)
    (restore continue)
    (assign val (op *) (reg n) (reg val))   ; val now contains n(n - 1)!
    (goto (reg continue))                   ; return to caller
   base-case
    (assign val (const 1))                  ; base case: 1! = 1
    (goto (reg continue))                   ; return to caller
   fact-done))

(define (factorial n)
  (if (< n 2)
      1
      (* n (factorial (- n 1)))))


(define (run-fact n)
  (let ((fact-machine
         (make-machine
          '(n val continue)
          (list (list '= =)
                (list '- -)
                (list '* *))
          controller)))
    (set-register-contents! fact-machine 'n n)
    (start fact-machine)
    ((get-stack fact-machine) 'print-statistics)
    (get-register-contents fact-machine 'val)))

(assert= (run-fact 2) (factorial 2))
(assert= (run-fact 3) (factorial 3))
(assert= (run-fact 4) (factorial 4))
(assert= (run-fact 5) (factorial 5))
(assert= (run-fact 6) (factorial 6))
(assert= (run-fact 7) (factorial 7))

