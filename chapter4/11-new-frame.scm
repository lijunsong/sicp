;;; new representation for frame

(load "3-data-directed-eval.scm")

;;; 1. 
(define (make-frame variables values)
  (map (lambda (x y) (cons x y)) variables values))

;;; 2. 
(define (add-binding-to-frame! var val frame)
  (define (last-pair lst)
    (if (or (null? lst) (null? (cdr lst)))
        lst
        (last-pair (cdr lst))))
  (set-cdr! (last-pair frame) (list (cons var val))))

;;; 3.
(define (lookup-variable-value var env)
  (define (lookup-frame frame) 
    (cond ((null? frame)
           (lookup-variable-value var (enclosing-environment env)))
          ((eq? var (car (car frame)))
           (cdr (car frame)))
          (else
           (lookup-frame (cdr frame)))))
  (if (eq? env the-empty-environment)
      (error 'lookup-variable-value "Unbound variable" var)
      (lookup-frame (first-frame env))))

;;; 4.
(define (set-variable-value! var val env)
  (define (lookup-and-set-frame frame)
    (cond ((null? frame)
           (set-variable-value! var val (enclosing-environment env)))
          ((eq? var (car (car frame)))
           (set-cdr! (car frame) val))
          (else
           (lookup-and-set-frame (cdr frame)))))
  (if (eq? env the-empty-environment)
      (error 'set-variable-value! "Unbound variable -- SET!" var)
      (lookup-and-set-frame (first-frame env))))

;;; 5.
(define (define-variable! var val env)
  (define (lookup-and-set-frame frame)
    (cond ((null? frame)
           (add-binding-to-frame! var val (first-frame env)))
          ((eq? var (car (car frame)))
           (set-cdr! (car frame) val))
          (else
           (lookup-and-set-frame (cdr frame)))))
  (lookup-and-set-frame (first-frame env)))

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
