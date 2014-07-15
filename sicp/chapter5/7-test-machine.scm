(load "machine-stack-module.scm")
(load "machine-register-module.scm")
(load "machine-machine-module.scm")
(load "machine-assemble-module.scm")

(define recursive-machine
  (make-machine
   '(continue n b val)
   (list (list '- -)
         (list '* *)
         (list '= =))
   
   '((assign continue (label expt-done))
     expt-loop
     (test (op =) (reg n) (const 0))
     (branch (label base-case))

     (save continue)
     (save n)
     (assign n (op -) (reg n) (const 1))
     (assign continue (label after-expt))
     (goto (label expt-loop))

     after-expt
     (restore n)
     (restore continue)
     (assign val (op *) (reg b) (reg val))
     (goto (reg continue))
     base-case
     (assign val (const 1))
     (goto (reg continue))
     expt-done)))
;; 12^3

(load "../testframe.scm")
(set-register-contents! recursive-machine 'n 3)
(set-register-contents! recursive-machine 'b 12)
(start recursive-machine)
(assert= (get-register-contents recursive-machine 'val)
         (* 12 12 12))


(define iterative-machine
  (make-machine
   '(n b counter product)
   (list (list '= =)
         (list '- -)
         (list '* *))
   '((assign counter (reg n))
     (assign product (const 1))
     expt-loop
     (test (op =) (reg counter) (const 0))
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
