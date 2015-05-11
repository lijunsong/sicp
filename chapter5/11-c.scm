(load "machine-stack-module.scm")
(load "machine-register-module.scm")
(load "machine-assemble-module.scm")

; rewrite the "machine-machine-module.scm"
; 1. associate one stack with each regs

; Part1
(define (make-new-machine regs)
                                        ; associate stack for each regs
  (let ((stacks (map (lambda (reg) (cons reg (make-stack))) regs))
        (flag (make-register))
        (pc (make-register))
        (the-instruction-sequence '()))
    (let ((regs (list (cons 'pc pc)
                      (cons 'flag flag)))
                                        ;
          (the-ops
           (list (list 'initialize-stack
                       (lambda () 
                         (map (lambda (stk) (stk 'initialize)) stacks))))))
      ; Part1 ends
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
              ((eq? msg 'stack) stacks)
              ((eq? msg 'operations) the-ops)
              ((eq? msg 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? msg 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              (else
               (error 'machine "Unknown message: " msg))))
      dispatch)))


; Part2
(define (make-machine regs ops controller-text)
  (let ((machine (make-new-machine regs)))  ; pass regs for stack init
    (for-each (lambda (name)
                ((machine 'allocate-register) name))
              regs)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))
; Part2 ends

; Part3
;; change machine-assemble-module.scm
(define (make-save inst machine stack pc)
  (let* ((name (stack-inst-reg-name inst))
         (reg (get-register machine name)))
    (lambda ()
      (let* ((stk (assoc name stack))) ; no need to have run time checking
        (begin
          (push (cdr stk) (get-contents reg))
          (advance-pc pc))))))

(define (make-restore inst machine stack pc)
  (let* ((name (stack-inst-reg-name inst))
         (reg (get-register machine name)))
    (lambda ()
      (let ((stk (assoc name stack)))
        (set-contents! reg (pop (cdr stk)))
        (advance-pc pc)))))
; Part3 ends

;; tests begin
(define test-controller
  '((save x)
    (save y)
    (save z)
    (assign x (const -1))
    (assign y (const -1))
    (assign z (const -1))

    (restore y)    
    (restore x)
    (restore z)))

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

