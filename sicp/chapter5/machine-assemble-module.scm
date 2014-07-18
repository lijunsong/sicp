;; extract-labels module will extract labels and corresponding
;; instructions.
;; the instructions consists of instruction text and instruction
;; procedure. Procedures will be set afterwards (see machine module).

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels
       (cdr text)
       (lambda (labels insts)
         (let ((next-inst (car text)))
           (cond ((and (symbol? next-inst) (assoc next-inst labels))
                  (error 'extract-labels "Duplicate labels: " next-inst))
                 ((symbol? next-inst)
                  (receive
                      (cons (make-label-entry next-inst insts) labels)
                      insts))
                 (else 
                  (receive
                      labels
                      (cons (make-instruction next-inst) insts)))))))))


(define (make-label-entry label-name insts)
  (cons label-name insts))
(define (make-instruction text)
  (cons text '()))
(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels))) 
    (if val
        (cdr val)
        (error 'label "Undefined label -- ASSEMBLE" label-name))))

(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))
(define (instruction-execution-proc inst)
  (cdr inst))

(load "../testframe.scm")
; NOTE: '((label1 ((inst1 arg)) ...)) the reason why use ((inst1 arg)) is that the instruction procedure will be stored in cdr of ((inst1 arg)) later 
(let ((text
       '(label1
         (inst1 arg)
         (inst2 arg)
         label2
         (inst3 arg)
         (inst4 arg)
         label3)))
  (let ((labels (extract-labels text (lambda (labels insts) labels)))
        (insts (extract-labels text (lambda (labels insts) insts))))
    (begin
      (assertequal?
       labels
       '((label1 ((inst1 arg)) ((inst2 arg)) ((inst3 arg)) ((inst4 arg)))
         (label2 ((inst3 arg)) ((inst4 arg)))
         (label3)))
      (assertequal?
       insts
       '(((inst1 arg))
         ((inst2 arg))
         ((inst3 arg))
         ((inst4 arg))))
      (assertequal? (lookup-label labels 'label1)
                    '(((inst1 arg)) ((inst2 arg)) ((inst3 arg)) ((inst4 arg)))))))

(load "machine-machine-module.scm")
;; assemble will construct procedure for instructions
;; it needs machines' register information.
(define (assemble constroller-text machine)
  (extract-labels constroller-text
                  (lambda (labels insts)
                    (update-insts! insts labels machine)
                    insts)))

(define (instruction-text inst)
  (car inst))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (get-stack machine))
        (ops (get-operations machine)))
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc!
        inst
        (make-execution-procedure
         (instruction-text inst) labels machine pc flag stack ops)))
     insts)))

;;;;;;;;;;;;;;;;;;;;;;;;;;; make procedures ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

(define (make-execution-procedure inst labels machine
                                  pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
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

;;;;;;;;;;;;;;;;;;;;;;;;;; detailed proc ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ASSIGN example:
; (assign t (op rem) (reg a) (reg b))
; (assign a (reg b))
(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))
(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))
(define (make-assign inst machine labels operations pc)
  (let ((target
         (get-register machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
           (if (operation-exp? value-exp)
               (make-operation-exp value-exp machine labels operations)
               (make-primitive-exp (car value-exp) machine labels))))
      (lambda ()
        (set-contents! target (value-proc))
        (advance-pc pc)))))

; TEST example:
; (test (op =) (reg b) (const 0))
(define (test-condition test-instruction)
  (cdr test-instruction))
(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
        (let ((condition-proc
               (make-operation-exp condition machine labels operations)))
          (lambda ()
            (set-contents! flag (condition-proc))
            (advance-pc pc)))
        (error 'make-test "Bad TEST instruction -- ASSEMBLE" inst))))

; BRANCH example:
; (branch (label gcd-done))
(define (branch-dest branch-instruction)
  (cadr branch-instruction))

(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
        (let ((insts (lookup-label labels (label-exp-label dest))))
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc insts)
                (advance-pc pc))))
        (error "Bad BRANCH instruction -- ASSEMBLE" inst))))

; GOTO example:
; (goto (label test-b))
(define (goto-dest goto-instruction)
  (cadr goto-instruction))
(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts
                  (lookup-label labels (label-exp-label dest))))
             (lambda ()
               (set-contents! pc insts))))
          ((register-exp? dest)
           (let ((reg
                  (get-register machine (register-exp-reg dest))))
             (lambda ()
               (set-contents! pc (get-contents reg)))))
          (else
           (error 'make-goto "Bad GOTO instruction -- ASSEMBLE" inst)))))

; SAVE example:
; (save <register-name>)
; RESTORE example:
; (restore <register-name>)

(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))
(define (make-save inst machine stack pc)
  (let ((reg (get-register machine (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))
(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))

; perform example:
; (perform (op <operation-name>) <input1>...<inputn>)
(define (perform-action inst)
  (cdr inst))
(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
        (let ((action-proc
               (make-operation-exp action machine labels operations)))
          (lambda ()
            (action-proc)
            (advance-pc pc)))
        (error "Bad PERFORM instruction -- ASSEMBLE" inst))))


;; the two important procedure
(define (register-exp? exp) (tagged-list? exp 'reg))
(define (register-exp-reg exp) (cadr exp))

(define (constant-exp? exp) (tagged-list? exp 'const))
(define (constant-exp-value exp) (cadr exp))

(define (label-exp? exp) (tagged-list? exp 'label))
(define (label-exp-label exp) (cadr exp))

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))
(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))
(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
        (cadr val)
        (error 'lookup-prim "Unknown operation -- ASSEMBLE" symbol))))

(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((label-exp? exp)
         (let ((insts (lookup-label labels (label-exp-label exp))))
           (lambda () insts)))
        ((register-exp? exp)
         (let ((r (get-register machine
                                (register-exp-reg exp))))
           (lambda () (get-contents r))))
        (else
         (error 'make-primitive-exp
                "Unknown expression type -- ASSEMBLE" exp))))

;; (define (make-operation-exp exp machine labels operations)
;;   (let ((op (lookup-prim (operation-exp-op exp) operations))
;;         (aprocs
;;          (map (lambda (e) (make-primitive-exp e machine labels))
;;               (operation-exp-operands exp))))
;;     ; apply the op to all operands
;;     (lambda ()
;;       (apply op (map (lambda (p) (p)) aprocs)))))

;; updated version
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

