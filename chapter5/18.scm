(load "machine-register-module.scm")
(load "machine-machine-module.scm")
(load "machine-assemble-module.scm")
(load "machine-new-stack-module.scm")

;; register module
(define (make-register reg-name)
  (let ((name reg-name)
        (content '*unassigned*)
        (trace-on #f))
    (define (set-content v)
      (if trace-on
          (begin
            (display name) (display ": ")
            (display content) (display " -> ") (display v)
            (newline)))
      (set! content v))
    (define (dispatch msg)
      (cond ((eq? msg 'get) content)
            ((eq? msg 'set) set-content)
            ((eq? msg 'trace-on) (set! trace-on #t))
            ((eq? msg 'trace-off) (set! trace-on #f))
            (else (error "UNKNOWN COMMAND -- make-register" message))))
    dispatch))

;; machine module
(define (make-new-machine)
  (let ((stack (make-stack))
        (flag (make-register 'flag))
        (pc (make-register 'pc))
        (the-instruction-sequence '()))
    (let ((regs (list (cons 'pc pc)
                      (cons 'flag flag))) ; assoc of name->register-object
          (the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize))))))
      (define (allocate-register name)
        (if (assoc name regs)
            (error 'machine "register exists: " name)
            (set! regs (cons (cons name (make-register name)) regs)))
        'register-allocated)
      (define (lookup-register name)
        (let ((r (assoc name regs)))
          (if r
              (cdr r)
              (error 'machine "Unknown register: " name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (cond ((null? insts) 'done)
                (else
                 ((instruction-execution-proc (car insts)))
                 (execute)))))

      ;; turn on or off register trace
      (define (set-register-trace name on-or-off)
        (let ((reg (assoc name regs)))
          (if reg
              ((cdr reg) on-or-off)
              (error 'trace-register "Unknown register: " name))))
      
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
              ;; add machine msg
              ((eq? msg 'trace-register-on)
               (lambda (name) (set-register-trace name 'trace-on)))
              ((eq? msg 'trace-register-off)
               (lambda (name) (set-register-trace name 'trace-off)))
              ;; end
              (else
               (error 'machine "Unknown message: " msg))))
      dispatch)))

(define (trace-register machine reg-name)
  ((machine 'trace-register-on) reg-name))

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

(define fact-machine
  (make-machine
   '(n val continue)
   (list (list '= =)
         (list '- -)
         (list '* *))
   controller))

(define (run-fact n)
  (trace-register fact-machine 'n)
  (set-register-contents! fact-machine 'n n)
  (start fact-machine)
  (get-register-contents fact-machine 'val))

(assert= (run-fact 5) (factorial 5))

