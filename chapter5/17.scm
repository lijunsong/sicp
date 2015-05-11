(load "machine-register-module.scm")
(load "machine-machine-module.scm")
(load "machine-assemble-module.scm")
(load "machine-new-stack-module.scm")

;; 1
(define (assemble constroller-text machine)
  (extract-labels constroller-text
                  (lambda (labels insts)
                    ;; labels contains useful information
                    (set-inst-label! labels machine)
                    (update-insts! insts labels machine)
                    insts)))
;; 2
(define (set-inst-label! labels machine)
  (define (make-inst-label labels-insts)
    (if (null? labels-insts)
        '()
        (let* ((label-insts (car labels-insts))
               (label (car label-insts))
               (insts (cdr label-insts)))
          (append (make-inst-label (cdr labels-insts))
                  (map (lambda (inst) (cons (car inst) label)) insts)))))
  ((machine 'install-label-info) (make-inst-label labels)))
;; end 2

(define (make-machine regs ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (name)
                ((machine 'allocate-register) name))
              regs)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

(define (make-new-machine)
  (let ((stack (make-stack))
        (flag (make-register))
        (pc (make-register))
        (the-instruction-sequence '())
        (inst-count 0)
        (trace-flag #f)
        ;; inst-label will be an assoc list mapping from an inst to
        ;; its preceding label
        (inst-label '()))
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
      (define (lookup-register name)
        (let ((r (assoc name regs)))
          (if r
              (cdr r)
              (error 'machine "Unknown register: " name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (cond ((null? insts) 'done)
                (else
                 (print-inst-label (caar insts))
                 (set! inst-count (+ 1 inst-count))
                 ((instruction-execution-proc (car insts)))
                 (execute)))))
      (define (reset)
        (set! inst-count 0))
      (define (print-inst-count)
        (newline)
        (display (list 'instructions '= inst-count)))
      ;; print the instruction and label
      (define (print-inst-label inst)
        (if trace-flag
            (begin
              (newline)
              (cond ((assv inst inst-label)
                     (display (cdr (assv inst inst-label)))
                     (display ": "))
                    (else
                     (display "NO LABEL: ")))
              (display inst))))
      ;; end
      (define (trace-on)
        (set! trace-flag #t))
      (define (trace-off)
        (set! trace-flag #f))
      
      (define (dispatch msg)
        (cond ((eq? msg 'allocate-register) allocate-register)
              ((eq? msg 'lookup-register) lookup-register)
              ((eq? msg 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? msg 'stack) stack)
              ((eq? msg 'operations) the-ops)
              ((eq? msg 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? msg 'install-label-info)
               (lambda (label-info) (set! inst-label label-info)))
              ((eq? msg 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? msg 'trace-on)
               (trace-on))
              ((eq? msg 'trace-off)
               (trace-off))
              ((eq? msg 'inst-label)
               inst-label)
              ((eq? msg 'reset) (reset))
              ((eq? msg 'print-inst-count) (print-inst-count))
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

(define fact-machine
  (make-machine
   '(n val continue)
   (list (list '= =)
         (list '- -)
         (list '* *))
   controller))

(define (run-fact n)
  (fact-machine 'reset)
  (set-register-contents! fact-machine 'n n)
  (fact-machine 'trace-on)
  (start fact-machine)
  (fact-machine 'print-inst-count)
  (get-register-contents fact-machine 'val))

(assert= (run-fact 2) (factorial 2))
(assert= (run-fact 3) (factorial 3))
(assert= (run-fact 4) (factorial 4))
(assert= (run-fact 5) (factorial 5))
(assert= (run-fact 6) (factorial 6))
(assert= (run-fact 7) (factorial 7))

