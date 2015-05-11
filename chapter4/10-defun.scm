
(load "3-data-directed-eval.scm")
(load "../testframe.scm")


;;; definition
;;; add defun, common-lisp syntax, to define a function in scheme
(define (install-eval-defun)
  ;; import 
  (define (make-lambda parameter body)
    ((get 'constructor lambda-tag) parameter body))

  ;; selector
  (define (defun-variable exp)
    (car exp))
  (define (defun-paramter exp)
    (cadr exp))
  (define (defun-body exp)
    (cddr exp))
  
  (define (defun-value exp)
    (make-lambda (defun-paramter exp)
                 (defun-body exp)))
  
  ;; main handler
  (define (eval-defun exp env)
    (define-variable! (defun-variable exp)
                      (eval (defun-value exp) env)
                      env)
    'ok)
  (put 'eval 'defun eval-defun))
;;; definition end

;;; tests begin

(let ((test-env (setup-environment)))
  (begin
    (install-eval-defun)
    (eval '(defun fib-iter (a b count)
             (if (= count 0)
                 b
                 (fib-iter (+ a b) a (+ count -1)))) test-env)
    (assert= (eval '(fib-iter 1 0 0) test-env) 0)
    (assert= (eval '(fib-iter 1 0 3) test-env) 2)))
 
