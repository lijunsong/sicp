(load "machine-register-module.scm")
(load "machine-machine-module.scm")
(load "machine-assemble-module.scm")
(load "machine-new-stack-module.scm")

;; install label-instructions
(define (make-machine regs ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (name)
                ((machine 'allocate-register) name))
              regs)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    ((machine 'install-labels)
     (get-labels controller-text machine))
    machine))

(define (get-labels constroller-text machine)
  (extract-labels constroller-text
                  (lambda (labels insts) labels)))

;; add breakpoints list, stop-the-machine flags to machine
(define (make-new-machine)
  (let ((stack (make-stack))
        (flag (make-register))
        (pc (make-register))
        (the-instruction-sequence '())
        (labels '())
        (breakpoints '()) ; assoc list of inst -> (label n)
        (stop-the-machine #t))
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
      ;; new execute
      (define (execute)
        (let ((insts (get-contents pc)))
          (cond ((null? insts) 'done)
                ((and stop-the-machine (hit-breakpoint? (caar insts)))
                 (list 'stop 'at (hit-breakpoint? (caar insts))))
                (else
                 ((instruction-execution-proc (car insts)))
                 (set! stop-the-machine #t)
                 (execute)))))
      ;; end
      ;; lookup instruction in label-instructions assoc list
      ;; return instruction
      (define (lookup-instruction label n)
        (define (lookup-inst label insts count)
          (cond ((and (not (= n count)) (null? insts))
                 (error 'lookup-instruction "Invalid " 
                        n "after label " label))
                ((= n count)
                 (caar insts))
                (else
                 (lookup-inst label (cdr insts) (+ 1 count)))))
        (let ((insts (assoc label labels)))
          (if insts
              (lookup-inst label (cdr insts) 1)
              (error 'lookup-instruction "Invalid label " label))))
      ;; end
      
      ;; breakpoint related
      ;; breakpoints list stores (((inst text1) label1 n1) ((inst text2) label2 n2))
      (define (set-breakpoint label n)
        (let ((inst (lookup-instruction label n)))
          (set! breakpoints (append breakpoints
                                    (list (cons inst (list label n)))))
          (list 'break 'at inst (list label n))))
      (define (cancel-breakpoint label n)
        (let ((inst (lookup-instruction label n)))
          (set! breakpoints (remove (lambda (x)
                                      (eq? inst (car x))) breakpoints))
          'cancelled))
      (define (hit-breakpoint? inst)
        (find (lambda (breakpoint)
                (eq? inst (car breakpoint)))
              breakpoints))
      (define (proceed-machine)
        (set! stop-the-machine #f)
        (execute))
      (define (cancel-all-breakpoints)
        (set! breakpoints '())
        '(all break points are cancelled))
      ;; end
      (define (dispatch msg)
        (cond ((eq? msg 'allocate-register) allocate-register)
              ((eq? msg 'lookup-register) lookup-register)
              ((eq? msg 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? msg 'stack) stack)
              ((eq? msg 'operations) the-ops)
              ((eq? msg 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? msg 'install-labels)
               (lambda (labls) (set! labels labls)))
              ((eq? msg 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              
              ((eq? msg 'set-breakpoint) set-breakpoint)
              ((eq? msg 'proceed-machine) (proceed-machine))
              ((eq? msg 'cancel-breakpoint) cancel-breakpoint)
              ((eq? msg 'cancel-all-breakpoints) (cancel-all-breakpoints))

              (else
               (error 'machine "Unknown message: " msg))))
      dispatch)))

;; helper functions
(define (set-breakpoint machine label n)
  ((machine 'set-breakpoint) label n))
(define (proceed-machine machine)
  (machine 'proceed-machine))
(define (cancel-breakpoint machine label n)
  ((machine 'cancel-breakpoint) label n))
(define (cancel-all-breakpoints machine)
  (machine 'cancel-all-breakpoints))

;; tests begin
(load "../testframe.scm")
(define gcd-machine
  (make-machine
   '(a b t)
   (list (list 'rem remainder) (list '= =))
   '(test-b
       (test (op =) (reg b) (const 0))
       (branch (label gcd-done))
       (assign t (op rem) (reg a) (reg b))
       (assign a (reg b))
       (assign b (reg t))
       (goto (label test-b))
     gcd-done)))

(set-register-contents! gcd-machine 'a 206)
(set-register-contents! gcd-machine 'b 40)

(set-breakpoint gcd-machine 'test-b 4)
(start gcd-machine)
(assert= (get-register-contents gcd-machine 'a) 206)
(assert= (get-register-contents gcd-machine 'b) 40)
(assert= (get-register-contents gcd-machine 't) 6)
(proceed-machine gcd-machine)
(assert= (get-register-contents gcd-machine 'a) 40)
(assert= (get-register-contents gcd-machine 'b) 6)
(assert= (get-register-contents gcd-machine 't) 4)
;(proceed-machine gcd-machine)
;(assert= (get-register-contents gcd-machine 'a) 6)
;(assert= (get-register-contents gcd-machine 'b) 4)
;(assert= (get-register-contents gcd-machine 't) 2)
(cancel-breakpoint gcd-machine 'test-b 4)
(proceed-machine gcd-machine)
(assert= (get-register-contents gcd-machine 'a) 2)
