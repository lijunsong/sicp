(load "machine-stack-module.scm")
(load "machine-register-module.scm")
(load "machine-machine-module.scm")
(load "machine-assemble-module.scm")

(define (make-execution-procedure inst labels machine
                                  pc flag stack ops)
  (cond ((or (eq? (car inst) 'assign) (eq? (car inst) 'mov))
         (make-assign inst machine labels ops pc))
        ((or (eq? (car inst) 'test) (eq? (car inst) 'cmp))
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        (else (error "Unknown instruction type -- ASSEMBLE"
                     inst))))

(define iterative-machine
  (make-machine
   '(n b counter product)
   (list (list '= =)
         (list '- -)
         (list '* *))
   '((mov counter (reg n))
     (mov product (const 1))
     expt-loop
     (cmp (op =) (reg counter) (const 0))
     (branch (label expt-done))
     
     (assign counter (op -) (reg counter) (const 1))
     (assign product (op *) (reg product) (reg b))
     (goto (label expt-loop))
     expt-done)))

(set-register-contents! iterative-machine 'n 3)
(set-register-contents! iterative-machine 'b 12)
(start iterative-machine)
(assert= (get-register-contents iterative-machine 'product)
         (* 12 12 12))
