(load "6-let.scm")

(load "12-abstract-operation-on-env.scm")

(install-eval-let) ;; install let support!

;;; a
;; a new lookup-variable-value
;; which can signal an error caused by unassigned variable
(define (lookup-variable-value var env)
  (let ((var-val (lookup-env var env)))
    (cond ((null? var-val)
           (error 'lookup-variable-value "Unbound variable" var))
          ((eq? (cdr var-val) '*unassigned*)
           (erorr 'lookup-variable-value "Assign to an unassigned variable" var))
          (else
           (cdr var-val)))))

;;; b
;; scan-out-defines will desugar internal defines statements.
(define (scan-out-defines body)
  ; selectors
  (define (definition? exp)
    (tagged-list? exp definition-tag))
  
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

  (define (make-let vars vals body)
    ((get 'constructor 'let) vars vals body))

  (define (make-assignments vars vals)
    (map (lambda (var val)
           (list assignment-tag var val))
         vars vals))

  ;; all the definition is raised to the top
  (define (collect-vars exps)
    (let* ((defs (filter definition? exps))
           (rest (filter (lambda (e)
                           (not (definition? e))) exps))
           (vars (map (lambda (e)
                        (definition-variable (cdr e))) defs))
           (vals (map (lambda (e)
                        (definition-value (cdr e))) defs)))
      (if (null? defs)
          exps
          (list
           (make-let vars
                     (map (lambda (x) ''*unassigned*) vars)
                     (append (make-assignments vars vals) rest))))))

  (collect-vars body))

;;; c.
;; install scan-out-defines in the system
(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))

;;; tests begin
(assertequal? (scan-out-defines (cddr '(lambda (x)
                                         (define u 1)
                                         (define v 2)
                                         3)))
              '((let ((u '*unassigned*)
                      (v '*unassigned*))
                  (set! u 1)
                  (set! v 2)
                  3)))

(assertequal? (scan-out-defines (cddr '(lambda (t)
                                         (define (inc x) (+ 1 x))
                                         (define (dec x) (+ -1 x))
                                         (inc (dec (inc t))))))
              '((let ((inc '*unassigned*)
                      (dec '*unassigned*))
                  (set! inc (lambda (x) (+ 1 x)))
                  (set! dec (lambda (x) (+ -1 x)))
                  (inc (dec (inc t))))))

(assertequal? (scan-out-defines (cddr '(lambda (t)
                                         (inc (dec (inc t)))
                                         (define (inc x) (+ 1 x))
                                         (define (dec x) (+ -1 x)))))
              '((let ((inc '*unassigned*)
                      (dec '*unassigned*))
                  (set! inc (lambda (x) (+ 1 x)))
                  (set! dec (lambda (x) (+ -1 x)))
                  (inc (dec (inc t))))))

(assertequal? (scan-out-defines (cddr '(lambda (t)
                                         (+ t 1)
                                         (+ t -1))))
              '((+ t 1)
                (+ t -1)))

;; test evaluation

(let ((test-env (setup-environment)))
  (assert= (eval '((lambda (x)
                           (define u 1)
                           (define v 2)
                           (+ u v x)) 1) test-env)
                4)

  (assert= (eval '((lambda (t)
                          (define (inc x) (+ 1 x))
                          (define (dec x) (+ -1 x))
                          (inc (dec (inc t))))
                        10) test-env)
                11)

  (assert= (eval '((lambda (t)
                          (inc (dec (inc t)))
                          (define (inc x) (+ 1 x))
                          (define (dec x) (+ -1 x)))
                        10) test-env)
                11)

  (assert= (eval '((lambda (t)
                          (+ t 1)
                          (+ t -1))
                        10) test-env)
                9))
