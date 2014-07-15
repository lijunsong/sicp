(load "machine-stack-module.scm")
(load "machine-register-module.scm")
(load "machine-assemble-module.scm")

; rewrite the "machine-machine-module.scm"
; 1. associate one stack with each regs
; 2. modify assemble module to push and pop the stack associated with
;    the register


(define test-controller
  (save x)
  (save y)
  (save z)
  (assign x (const -1))
  (assign y (const -1))
  (assign z (const -1))

  (restore x)
  (restore y)
  (restore z))

(define test-machine
  (make-machine
   '(x y z)
   (list (list))
   test-controller))

(set-register-contents! test-machine 'x 1)
(set-register-contents! test-machine 'y 2)
(set-register-contents! test-machine 'z 3)
(start test-machine)
(assert= (get-register-contents test-machine 'x) 1)
(assert= (get-register-contents test-machine 'y) 2)
(assert= (get-register-contents test-machine 'z) 3)
