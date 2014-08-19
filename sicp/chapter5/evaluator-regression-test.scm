;(load "evaluator.scm")
(load "../testframe.scm")

;;; self-evaluating
(assert= (eval 1 the-global-environment) 1)
(assertequal? (eval "str" the-global-environment) "str")

;; definition
(eval '(define var1 1) the-global-environment)
(assert= (eval 'var1 the-global-environment) 1)

(eval '(define var-special false) the-global-environment) ;; for testing of env lookup 
(asserteq? (eval 'var-special the-global-environment) false)

(eval '(define (empty? lst) (null? lst))
      the-global-environment)

(eval '(define (new-append x y)
         (if (null? x)
             y
             (cons (car x)
                   (new-append (cdr x) y)))) the-global-environment)

(asserteq? (eval '(empty? '(1 2)) the-global-environment)
           false)
(assertequal? (eval '(new-append '(1 2) '(3 4)) the-global-environment)
              '(1 2 3 4))

;; assignment
(asserteq? (eval '(set! var1 2) the-global-environment) 'ok)
(assert= (eval 'var1 the-global-environment) 2)

(assert/exn (eval 'var2 the-global-environment) "Unbound")
(assert/exn (eval '(set! var2 34) the-global-environment) "Unbound")

;; if
(assert= (eval '(if 0 1 2) the-global-environment) 1)
(assert= (eval '(if 'x 1 2) the-global-environment) 1)
(assert= (eval '(if '() 1 2) the-global-environment) 1)
(assert= (eval '(if false 1 2) the-global-environment) 2)

;; cond
(assert= (eval '(cond ((= var1 2) 19)) the-global-environment) 19)
(assert= (eval '(cond ((= var1 1) 19)
                      ((= var1 2) 20)) the-global-environment) 20)
(asserteq? (eval '(cond ((= var1 1) 19)) the-global-environment) false)


;; lambda
(asserteq? (eval '((lambda (x) (= x 1)) 1) the-global-environment) true)

;; begin
(assert= (eval '(begin (set! var1 14)
                       (set! var1 (+ var1 1))
                       var1) the-global-environment) 15)
