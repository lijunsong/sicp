(load "evaluator.scm")

(define (apply1 procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env)))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-delayed-args arguments env)
           (procedure-environment procedure))))
        (else
         (error 'apply1
                "Unknown procedure type -- APPLY" procedure))))


(define (actual-value exp env)
  (force-it (eval exp env)))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps)
                                env))))
(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps) env)
            (list-of-delayed-args (rest-operands exps)
                                  env))))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp env))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) 
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)  
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp) 
         (apply1 (actual-value (operator exp) env)
                 (operands exp)
                 env))
        (else
         (error 'eval "Unknown expression type -- EVAL" exp))))

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))


(define input-prompt ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output
           (actual-value input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (delay-it exp env)
  (list 'thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-exp thunk) (cadr thunk))

(define (thunk-env thunk) (caddr thunk))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))
(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value
                        (thunk-exp obj)
                        (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)  ; replace exp with its value
           (set-cdr! (cdr obj) '())     ; forget unneeded env
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))

;;; lazy stream

(let ((statements
       (list
        '(define (cons x y)
          (lambda (m) (m x y)))
        '(define (car z)
          (z (lambda (p q) p)))
        '(define (cdr z)
          (z (lambda (p q) q)))

        '(define (list-ref items n)
          (if (= n 0)
              (car items)
              (list-ref (cdr items) (+ n -1))))
        '(define (map proc items)
          (if (null? items)
              '()
              (cons (proc (car items))
                    (map proc (cdr items)))))
        '(define (scale-list items factor)
          (map (lambda (x) (* x factor))
               items))
        '(define (add-lists list1 list2)
          (cond ((null? list1) list2)
                ((null? list2) list1)
                (else (cons (+ (car list1) (car list2))
                            (add-lists (cdr list1) (cdr list2))))))
        '(define ones (cons 1 ones))
        '(define integers (cons 1 (add-lists ones integers))))))
  (for-each (lambda (x)
              (eval x the-global-environment))
            statements))

;; new text-of-quotation

(define (text-of-quotation exp env)
  (let ((text (cadr exp)))
    (if (pair? text)
        (eval (quote-list->cons text) env)
        text)))

;; convert a list to cons construction
(define (quote-list->cons lst)
  (cond ((null? lst) '())
        ((not (pair? lst)) lst)
        (else
         (list 'cons (list 'quote (car lst)) (quote-list->cons (cdr lst))))))

;; end
