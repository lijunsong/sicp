;;; new representation for frame

(load "3-data-directed-eval.scm")
(load "11-new-frame.scm")

;;; lookup in all the frames
(define (lookup-env var env)
  (define (lookup-frame frame)
    (cond ((null? frame)
           (lookup-env var (enclosing-environment env)))
          ((eq? var (car (car frame)))
           (car frame))
          (else
           (lookup-frame (cdr frame)))))
  (if (eq? env the-empty-environment)
      '()
      (lookup-frame (first-frame env))))

;;; define the three operations

(define (lookup-variable-value var env)
  (let ((var-val (lookup-env var env)))
    (if (null? var-val)
        (error 'lookup-variable-value "Unbound variable" var)
        (cdr var-val))))


(define (set-variable-value! var val env)
  (let ((var-val (lookup-env var env)))
    (if (null? var-val)
        (error 'set-variable-value! "Unbound variable -- SET!" var)
        (set-cdr! var-val val))))

(define (define-variable! var val env)
  (let ((var-val (lookup-env var env)))
    (if (null? var-val)
        (add-binding-to-frame! var val (first-frame env))        
        (set-cdr! var-val val))))

;;; reset the environment
(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))


(define the-global-environment (setup-environment))
