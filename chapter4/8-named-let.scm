(load "3-data-directed-eval.scm")
(load "../testframe.scm")

;;; let
(define (install-eval-named-let)
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
  

  (define (named-let? exp)
    (symbol? (car exp)))
  
  (define (let-bindings exp)
    (car exp))
  (define (let-body exp)
    (cdr exp))
  
  (define (let-vars bindings)
    (map (lambda (x) (car x)) bindings))
  
  (define (let-vals bindings)
    (map (lambda (x) (cadr x)) bindings))

  ;; 1. convert named-let to let
  (define (named-let->let exp)
    (let ((var (car exp))
          (bindings (cadr exp))
          (body (cddr exp)))

      (let ((procedure (make-lambda (let-vars bindings)
                                    body)))
        (make-let (list var)
                  (list 1) ; arbitrary value!
                  ((get 'constructor sequence-tag)
                   (list (list 'set! var procedure)
                         (cons var (let-vals bindings))))))))
  ; end 1
  
  ;;; 2. let->combination supports named let now
  (define (let->combination exp)
    (if (named-let? exp)
        (let->combination (cdr (named-let->let exp)))
        (cons (make-lambda (let-vars (let-bindings exp))
                           (let-body exp))
              (let-vals (let-bindings exp)))))
  ;;; end 2
  
  (define (eval-let exp env)
    (eval (let->combination exp) env))

  (put 'eval 'let eval-let)
  (put 'constructor 'let make-let))

;;; tests begin
; test the transformation

(let ((test-env (setup-environment)))
  (begin
    (install-eval-named-let)
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
    (eval '(define (fib n)
             (let fib-iter ((a 1)
                            (b 0)
                            (count n))
               (if (= count 0)
                   b
                   (fib-iter (+ a b) a (+ count -1))))) test-env)
    (assert= (eval '(fib 10) test-env) 55)))

