(load "3-data-directed-eval.scm")
(load "../testframe.scm")

;;; cond
(define (install-eval-new-cond)

  (define (eval-cond exp env)
    (eval (expand-clauses exp) env))

  (define (cond-else-clause? clause)
    (eq? (cond-predicate clause) 'else))
  
  (define (cond-arrow-clause? clause)
    (and (= 3 (length clause)) (eq? (cadr clause) '=>)))
  
  ; selector
  (define (cond-predicate clause) (car clause))
  (define (cond-actions clause) (cdr clause))
  (define (cond-arrow-action clause)
    (caddr clause))
  
  (define (sequence->exp seq)
    ((get 'export-1 sequence-tag) seq))
  
  (define (make-if tst thn els)
    ((get 'constructor if-tag) tst thn els))

  (define (make-lambda p b)
    ((get 'constructor lambda-tag) p b))

  ;; modified expand
  ;; supports the extended syntax
  (define (expand-clauses clauses)
    (if (null? clauses)
        'false                          ; no else clause
        (let ((first (car clauses))
              (rest (cdr clauses)))
          (cond ((and (cond-else-clause? first) (null? rest))
                 (sequence->exp (cond-actions first)))
                ((cond-else-clause? first)
                 (error "ELSE clause isn't last -- COND->IF"
                         clauses))
                ((cond-arrow-clause? first)
                 (list
                  (make-lambda (list 'v)
                               (list (make-if 'v
                                              (list (cond-arrow-action first) 'v)
                                              (expand-clauses rest))))
                  (cond-predicate first)))
                (else
                 (make-if (cond-predicate first)
                          (sequence->exp (cond-actions first))
                          (expand-clauses rest))                 
                 )))))
  (put 'eval cond-tag eval-cond))
;;; cond end


;;; tests begin

(let ((test-env (setup-environment)))
  (begin
    (install-eval-new-cond)
    (eval '(define (assoc key lst)
             (cond ((null? lst) false)
                   ((eq? key (car (car lst)))
                    (car lst))
                   (else (assoc key (cdr lst))))) test-env)
    (asserteq? (eval '(assoc 'b '()) test-env) false)
    (asserteq? (eval '(assoc 'b '((a 1) (c 1))) test-env) false)
    (assertequal? (eval '(assoc 'b '((a 1) (b 2) (c 1))) test-env) '(b 2))    
    (assert= (eval '(cond ((assoc 'b '((a 1) (b 2))) => (lambda (x) (car (cdr x))))
                          (else false))
                     test-env)
             2)
    (assert= (eval '(cond ((assoc 'b '((a 1) (c 2))) => (lambda (x) (car (cdr x))))
                          (else 3))
                     test-env)
             3)

    (eval '(define x 2) test-env)
    (eval '(define y 3) test-env)
    (eval '(define (inc x) (+ x 1)) test-env)
    (eval '(define (predict-set?)
             (set! y (+ y 1))
             y) test-env)
    (assert= (eval '(cond ((= x 1) (+ x 1))
                          ((predict-set?) => inc)
                          (else 10)) test-env)
             5)
    
    ))
