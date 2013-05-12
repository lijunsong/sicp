(load "6-let.scm")


(define (install-eval-letrec)
  (install-eval-let) ;; install let support!  
  (define (make-lambda parameter body)
    ((get 'constructor lambda-tag) parameter body))

  (define (make-let vars vals body)
    ((get 'constructor 'let) vars vals body))

  (define (make-assignments vars vals)
    (map (lambda (var val)
           (list assignment-tag var val))
         vars vals))

  (define (letrec-binding exp)
    (car exp))

  (define (letrec-body exp)
    (cdr exp))

  (define (letrec->let exp)
    (let* ((bindings (letrec-binding exp))
           (vars (map (lambda (x) (car x)) bindings))
           (vals (map (lambda (x) (cadr x)) bindings))
           (body (letrec-body exp)))
      (make-let vars
                (map (lambda (x) ''*unassigned*) vars)
                (append (make-assignments vars vals) body))))

  (define (eval-letrec exp env)
    (eval (letrec->let exp) env))
  
  (put 'eval 'letrec eval-letrec)
)


;;; tests begin
(load "../testframe.scm")

(let ((test-env (setup-environment)))
  (install-eval-letrec)

  (eval '(define (f x)
           (letrec ((even?
                     (lambda (n)
                       (if (= n 0)
                           true
                           (odd? (+ n -1)))))
                    (odd?
                     (lambda (n)
                       (if (= n 0)
                           false
                           (even? (+ n -1))))))
             (even? x)))
        test-env)
  (asserteq? (eval '(f 10) test-env) #t)
  (asserteq? (eval '(f 11) test-env) #f)
  (add-binding-to-frame! '* (list 'primitive *) (first-frame test-env))
  (assert= (eval '(letrec ((fact
                            (lambda (n)
                              (if (= n 1)
                                  1
                                  (* n (fact (+ n -1)))))))
                    (fact 10))
                 test-env)
           3628800))
