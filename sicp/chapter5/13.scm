(load "machine-stack-module.scm")
(load "machine-register-module.scm")
(load "machine-assemble-module.scm")

;; change1
(define (make-machine ops controller-text)
  (let ((machine (make-new-machine)))
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))
;; end change1

(define (make-new-machine)
  (let ((stack (make-stack))
        (flag (make-register))
        (pc (make-register))
        (the-instruction-sequence '()))
    (let ((regs (list (cons 'pc pc)
                      (cons 'flag flag))) ; assoc of name->register-object
          (the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize))))))
      (define (allocate-register name)
        (if (assoc name regs)
            (error 'machine "register exists: " name)
            (set! regs (cons (cons name (make-register)) regs)))
        'register-allocated)
      ;; change2
      (define (lookup-register name)
        (let ((r (assoc name regs)))
          (if r
              (cdr r)
              (begin
                (allocate-register name)
                (cdr (assoc name regs))))))
      ;; end change2
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

;; tests start
(load "../testframe.scm")

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
   (restore n)
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
   (list (list '< <)
         (list '- -)
         (list '+ +))
   test-controller))

(define (fib n)
    (if (< n 2)
        n
        (+ (fib (- n 1)) (fib (- n 2)))))

(set-register-contents! test-machine 'n 10)
(start test-machine)
(assert= (get-register-contents test-machine 'val)
         (fib 10))

