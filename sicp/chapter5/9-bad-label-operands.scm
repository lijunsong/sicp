(load "machine-stack-module.scm")
(load "machine-register-module.scm")
(load "machine-machine-module.scm")
(load "machine-assemble-module.scm")

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
         (map (lambda (e)
                (cond ((label-exp? e)
                       (error 'make-operation-exp "Bad expression type " e))
                      (else
                       (make-primitive-exp e machine labels))))
              (operation-exp-operands exp))))
    ; apply the op to all operands
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))


(load "../testframe.scm")
(assert/exn
  (make-machine '(n) (list (list '- -))
   '((assign n (op -) (label after-expt) (const 1))
     after-expt
     expt-done))
  "Bad expression")
