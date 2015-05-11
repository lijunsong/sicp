(load "6-let.scm")


(define (install-eval-let*)
  ;;import let
  (install-eval-let)
  (define (make-let vars vals body)
    ((get 'constructor 'let) vars vals body))

  (define (let*-body exp)
    (cdr exp))

  (define (let*-bindings exp)
    (car exp))

  (define (let*-vars bindings)
    (map (lambda (x) (car x)) bindings))
  (define (let*-vals bindings)
    (map (lambda (x) (cadr x)) bindings))
  
  (define (let*->nested-lets exp)
    (define (combine bindings body)
      (if (null? bindings)
          body
          (list ;; here we use list to construct let's body.
           (make-let (let*-vars (list (car bindings)))
                     (let*-vals (list (car bindings)))
                     (combine (cdr bindings) body)))))
    (car (combine (let*-bindings exp)
                  (let*-body exp))))
  
  (define (eval-let* exp env)
    (eval (let*->nested-lets exp) env))

  (put 'eval 'let* eval-let*))

;;; tests begin
(let ((test-env (setup-environment)))
  (install-eval-let)
  (install-eval-let*)
  (assert= (eval '(let* ((x 3)
                         (y (+ x 2))
                         (z (+ x y 5)))
                    (+ x z)) test-env)
           16)
  (eval '(define (f) (lambda (x) x)) test-env)
  (assert= (eval '(let* ((x 3)
                         (y (+ x 3)))
                    ((f) y) ;; this this a single statment!
                    )
                 test-env)
           6)
  (eval '(define (f2) (lambda () 3)) test-env)
  (assert= (eval '(let* ((x 3)
                         (y (+ x 3)))
                    ((f2)) ;; this this a single statment!
                    ((f) y)
                    (+ ((f) y) ((f2)))
                    )
                 test-env)
           9)

  )



