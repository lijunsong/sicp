(load "6-let.scm")
(load "11-new-frame.scm") 

(define (install-eval-unbound)
  (define (remove-var var env)
    (define (lookup-frame frame prevous-pointer)
      (cond ((null? frame)
             (remove-var var (enclosing-environment env)))
            ((and (eq? var (car (car frame)))
                  (eq? frame (first-frame env))) ;; the head pointer
             (set-car! env (cdr frame)))
            ((eq? var (car (car frame)))
             (set-cdr! prevous-pointer (cdr frame)))
            (else
             (lookup-frame (cdr frame) frame))))
    (if (eq? env the-empty-environment)
        (error 'remove-var "MAKE-UNBOUND ERROR--NOT FOUND " var)
        (lookup-frame (first-frame env) (first-frame env))))
  (define (eval-unbound exp env)
    (remove-var (car exp) env))

  (put 'eval 'make-unbound! eval-unbound))


;;; tests begin
(load "../testframe.scm")

(let ((test-env (setup-environment)))
  (install-eval-unbound)
  (install-eval-let)
  (assert= (eval '(let ((x 1))
                    (let ((x 2))
                      (let ((x 3))
                        (begin
                          (make-unbound! x)
                          (+ x 2))))) test-env)
           4)
  (assert= (eval '(let ((x 1))
                    (let ((x 2))
                      (let ((y x))
                        (make-unbound! x)
                        y))) test-env)
           2)
  (assert/exn (eval '(let ((x 1))
                       (make-unbound! x)
                       (+ x 1)) test-env)
              "Unbound")
  ;; TODO: test on define.
  )
