;;; install "and" and "or" as special form

(load "3-data-directed-eval.scm")
(load "../testframe.scm")

;;; evaluate and
(define (install-eval-and)
  (define (last-exp? e) (empty-exp? (cdr e)))
  (define (empty-exp? e) (null? e))
  
  (define (eval-and-sequence es env)
    (cond ((last-exp? es) (eval (car es) env))
          (else
           (if (false? (eval (car es) env))
               false
               (eval-and-sequence (cdr es) env)))))
  
  (define (eval-and exp env)
    (cond ((empty-exp? exp) true)
          (else
           (eval-and-sequence exp env))))  

  (put 'eval 'and eval-and))

;;; and end

;;; evaluate or
(define (install-eval-or)
  (define (last-exp? e) (empty-exp? (cdr e)))
  (define (empty-exp? e) (null? e))
  
  (define (eval-or-sequence es env)
    (cond ((last-exp? es) (eval (car es) env))
          (else
           (let ((v (eval (car es) env)))
             (if (true? v)
                 v
                 (eval-or-sequence (cdr es) env))))))
  
  (define (eval-or exp env)
    (cond ((empty-exp? exp) false)
          (else
           (eval-or-sequence exp env))))
  (put 'eval 'or eval-or))
;;; or end

;;; tests begin
(let ((test-env (setup-environment)))
  (begin
    (install-eval-and)
    (eval '(define var1 1) test-env)
    (assert= (eval '(and 1 2 3 4 var1) test-env) 1)
    (asserteq? (eval '(and 1 2 false 3 4) test-env) false)
    (eval '(and (begin (set! var1 (+ var1 1)) var1)
                (begin (set! var1 (+ var1 1)) var1)
                false
                (begin (set! var1 (+ var1 1)) var1))
          test-env)
    (assert= (eval 'var1 test-env) 3)))


(let ((test-env (setup-environment)))
  (begin
    (install-eval-or)
    (eval '(define var1 1) test-env)
    (assert= (eval '(or 4 var1) test-env) 4)
    (assert= (eval '(or false false false 3 4) test-env) 3)
    (assert= (eval '(or  (begin (set! var1 (+ var1 1)) false)
                         (begin (set! var1 (+ var1 1)) false)
                         var1
                         (begin (set! var1 (+ var1 1)) false))
                   test-env)
             3)))

;;; tests end
