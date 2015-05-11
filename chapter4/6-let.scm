(load "3-data-directed-eval.scm")
(load "../testframe.scm")

;;; let
(define (install-eval-let)
  ;; import make-lambda
  (define (make-lambda p b)
    ((get 'constructor lambda-tag) p b))
  
  ;; export constructor
  ;; NOTE: let's body passed in should be a sequence,
  ;; which means it should be a list!
  (define (make-let vars vals body)
    (append (list 'let
                  (map (lambda (x y) (list x y)) vars vals))
            body))
  (define (let-bindings exp)
    (car exp))
  (define (let-body exp)
    (cdr exp))
  
  (define (let-vars bindings)
    (map (lambda (x) (car x)) bindings))
  (define (let-vals bindings)
    (map (lambda (x) (cadr x)) bindings))
  
  (define (let->combination exp)
    (cons (make-lambda (let-vars (let-bindings exp))
                       (let-body exp))
          (let-vals (let-bindings exp))))
  
  (define (eval-let exp env)
    (eval (let->combination exp) env))

  (put 'eval 'let eval-let)
  (put 'constructor 'let make-let))

;;; tests begin
; test the transformation

(let ((test-env (setup-environment)))
  (begin
    (install-eval-let)
    (assert= (eval '(let ((x 1) (y 1))
                      (+ x y)) test-env)
             2)
    (assert= (eval '(let ((x 1))
                         (let ((x 2))
                           (+ x 2))) test-env)
             4)
    (assert= (eval '(let ((x 1))
                      (let ((x 2))
                        (set! x 3))
                      x) test-env)
             1)
    ))
