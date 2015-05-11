(load "3-data-directed-eval.scm")
(load "../testframe.scm")

;;; evaluate and

(define (install-eval-derived-and)
  (define (last-exp? e) (empty-exp? (cdr e)))
  (define (empty-exp? e) (null? e))

  ;; import make-if
  (define (make-if tst thn els)
    ((get 'constructor if-tag) tst thn els))
  
  (define (and->if exps)
    (if (last-exp? exps)
        (car exps)
        (make-if (car exps)
                 (and->if (cdr exps))
                 'false)))

  (define (eval-and exp env)
    (cond ((empty-exp? exp) true)
          (else
           (eval (and->if exp) env))))
  
  (put 'eval 'and eval-and))
;;; and end

;;; evaluate or

(define (install-eval-derived-or)
  (define (last-exp? e) (empty-exp? (cdr e)))
  (define (empty-exp? e) (null? e))
  
  ;; import make-lambda and make-if
  (define (make-lambda p b)
    ((get 'constructor lambda-tag) p b))
  
  (define (make-if tst thn els)
    ((get 'constructor if-tag) tst thn els))
  
  (define (or->if-lambda exps)
    (if (last-exp? exps)
        (car exps)
        (list (make-lambda '(v)
                           (list
                            (make-if 'v 'v (or->if-lambda (cdr exps)))))
              (car exps))))

  (define (eval-or exp env)
    (cond ((empty-exp? exp) false)
          (else
           (eval (or->if-lambda exp) env))))
  
  (put 'eval 'or eval-or))

;;; or end

;;; tests begin
(let ((test-env (setup-environment)))
  (begin
    (install-eval-derived-and)
    (eval '(define var1 1) test-env)
    (assert= (eval '(and 1 2 3 4 var1) test-env) 1)
    (asserteq? (eval '(and 1 2 false 3 4) test-env) false)
    (eval '(and (begin (set! var1 (+ var1 1)) var1)
                (begin (set! var1 (+ var1 1)) var1)
                false
                (begin (set! var1 (+ var1 1)) var1))
          test-env)
    (assert= (eval 'var1 test-env) 3)))

;;; or tests begin
(let ((test-env (setup-environment)))
  (begin
    (install-eval-derived-or)
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

;;; derived form below
; (and v1 v2 v3) => (if v1 (if v2 (if v3 v3 false) false) false)
; (or v1 v2 v3) => ((lambda (v) (if v v (...))) v1)


