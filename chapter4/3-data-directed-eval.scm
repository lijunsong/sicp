;;; data-directed evaluator
(load "get-put.scm")

;;;;;;;; for debug ;;;;;;;;
(define debug true)
(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(define (dp . exp)
  (if debug (for-each user-print exp))
  (newline))

;;;;;;;; debug end ;;;;;;
;; tags
(define quote-tag 'quote)
(define if-tag 'if)
(define cond-tag 'cond)
(define lambda-tag 'lambda)
(define sequence-tag 'begin)
(define assignment-tag 'set!)
(define definition-tag 'define)
(define application-tag '%%application)
; NOTE: application does not have tag on it.

(define apply-in-underlying-scheme apply)

(define (apply1 procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         ((get 'eval sequence-tag)
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

;; eval
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        (else
         (let ((eval-action (get 'eval (type-of-expression exp))))
           (cond (eval-action (eval-action (datum-of-expression exp) env))
                 ((pair? exp)
                  ((get 'eval application-tag) exp env)) ; hopefully this will not conflict 
                 (else (error "Unknown expression type -- EVAL" exp)))))))


(define (type-of-expression exp) (car exp))
(define (datum-of-expression exp) (cdr exp))

;; eval end

;;;;;;;;;;;;;;;;;;;;;;; procedures needed for run the eval
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))


;;; testing of predicates
(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))

;;; compound procedure
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

;;; primitive procedure
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'eq? eq?)
        (list '= =)
        (list '+ +)))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))


;;;;;;;;;;;;;;;;;;;;;;; environment ;;;;;;;;;;;;;;;;;;;;;;;;
(load "eval-env.scm")

;;;;;;;;;;;;;;;;;;;;;;; package ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; quote
(define (install-eval-quote)
  ;; procedure helper
  (define (text-of-quotation exp) (car exp))
  
  ;; main handler
  (define (eval-quote exp env)
    (text-of-quotation exp))
  ;; install
  (put 'eval quote-tag eval-quote))
;;; quote end

;;; assignment
(define (install-eval-assignment)
  ;; procedure helper
  (define (assignment-variable exp) (car exp))
  (define (assignment-value exp) (cadr exp))
  ;; lack set-variable-value!
  
  ;; main handler
  (define (eval-assignment exp env)
    (set-variable-value! (assignment-variable exp)
                         (eval (assignment-value exp) env)
                         env)
    'ok)
  (put 'eval assignment-tag eval-assignment))
;;; assignment end

;;; definition
(define (install-eval-definition)
  ;; procedure helper

  (define (definition-variable exp)
    (if (symbol? (car exp))
        (car exp)
        (caar exp)))
  (define (definition-value exp)
    (if (symbol? (car exp))
        (cadr exp)
        (make-lambda (cdar exp)   ; formal parameters
                     (cdr exp)))) ; body
  (define (make-lambda parameter body)
    ((get 'constructor lambda-tag) parameter body))
  ;; main handler
  (define (eval-definition exp env)
    (define-variable! (definition-variable exp)
                      (eval (definition-value exp) env)
                      env)
    'ok)
  (put 'eval definition-tag eval-definition))
;;; definition end

;;; if
(define (install-eval-if)
  ;; procedure helper
  (define (if-predicate exp) (car exp))
  (define (if-consequent exp) (cadr exp))
  (define (if-alternative exp)
    (if (not (null? (cddr exp)))
        (caddr exp)
        'false))
  (define (make-if predicate consequent alternative)
    (list 'if predicate consequent alternative))

  ;; main handler
  (define (eval-if exp env)
    (if (true? (eval (if-predicate exp) env))
        (eval (if-consequent exp) env)
        (eval (if-alternative exp) env)))
  
  (put 'eval if-tag eval-if)
  (put 'constructor if-tag make-if))
;;; if end

;;; lambda
(define (install-eval-lambda)
  (define (lambda-parameters exp) (car exp))
  (define (lambda-body exp) (cdr exp))
  (define (make-lambda parameters body)
    (cons lambda-tag (cons parameters body)))

  (define (eval-lambda exp env)
    (make-procedure (lambda-parameters exp)
                    (lambda-body exp)
                    env))
  ;;
  (put 'eval lambda-tag eval-lambda)
  (put 'constructor lambda-tag make-lambda))
;;; lambda end

;;; sequences
(define (install-eval-sequence)
  (define (last-exp? seq) (null? (cdr seq)))
  (define (first-exp seq) (car seq))
  (define (rest-exps seq) (cdr seq))
  (define (sequence->exp seq)
    (cond ((null? seq) seq)
          ((last-exp? seq) (first-exp seq))
          (else (make-begin seq))))
  (define (make-begin seq) (cons 'begin seq))
  
  ;; main handler
  (define (eval-sequences exps env)
    (cond ((last-exp? exps) (eval (first-exp exps) env))
          (else (eval (first-exp exps) env)
                (eval-sequences (rest-exps exps) env))))
  
  (put 'eval sequence-tag eval-sequences)
  (put 'constructor sequence-tag make-begin)
  (put 'export-1 sequence-tag sequence->exp))

;;; sequences end

;;; cond
(define (install-eval-cond)

  (define (eval-cond exp env)
    (eval (expand-clauses exp) env))
  
  (define (cond-else-clause? clause)
    (eq? (cond-predicate clause) 'else))
  (define (cond-predicate clause) (car clause))
  (define (cond-actions clause) (cdr clause))

  (define (sequence->exp seq)
    ((get 'export-1 sequence-tag) seq))
  
  (define (make-if tst thn els)
    ((get 'constructor if-tag) tst thn els))
  
  (define (expand-clauses clauses)
    (if (null? clauses)
        'false                          ; no else clause
        (let ((first (car clauses))
              (rest (cdr clauses)))
          (if (cond-else-clause? first)
              (if (null? rest)
                  (sequence->exp (cond-actions first))
                  (error "ELSE clause isn't last -- COND->IF"
                         clauses))
              (make-if (cond-predicate first)
                       (sequence->exp (cond-actions first))
                       (expand-clauses rest))))))
  (put 'eval cond-tag eval-cond))
;;; cond end

;;; application
(define (install-eval-application)
  (define (operator exp) (car exp))
  (define (operands exp) (cdr exp))
  (define (no-operands? ops) (null? ops))
  (define (first-operand ops) (car ops))
  (define (rest-operands ops) (cdr ops))

  (define (list-of-values exps env)
    (if (no-operands? exps)
        '()
        (cons (eval (first-operand exps) env)
              (list-of-values (rest-operands exps) env))))
  
  (define (eval-application exp env)
    (apply1 (eval (operator exp) env)
            (list-of-values (operands exp) env)))
  
  (put 'eval application-tag eval-application))
;;; application end

;;;;;;;;;;;;;;;;;; install modules (and test)
(install-eval-if)
(install-eval-cond)
(install-eval-quote)
(install-eval-lambda)
(install-eval-definition)
(install-eval-assignment)
(install-eval-sequence)
(install-eval-application)


;; (eval '(define (f x)
;;          (define (even? n)
;;            (if (= n 0)
;;                true
;;                (odd? (+ n -1))))
;;          (define (odd? n)
;;            (if (= n 0)
;;                false
;;                (even? (+ n -1))))
;;          (+ x 1)) the-global-environment)

