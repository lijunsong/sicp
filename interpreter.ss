(load "pmatch.scm")

;;; handling
;;; boolean numbers variables lambda application zero? sub1 * if
;;; implemented as data structure

;;; env is implemented as associated list
(define env '())
(define extend-env
  (lambda (names vals env)
    (cond [(null? names) env]
          [else
           (cons
            (cons (car names) (car vals))
            (extend-env (cdr names) (cdr vals) env))])))
;;; helper function to extend env using variables binded in let
;;; this implementation is sort of unefficient.
(define let-extend-env
  (lambda (let-binding env)
    (cond [(null? let-binding) env]
          [else
            (let-extend-env
             (cdr let-binding)
             (extend-env (list (car (car let-binding)))
                         (list (value-of (cadr (car let-binding)) env))
                         env))])))
(define lookup-in-env
  (lambda (name env)
    (let [(val (assoc name env))]
      (cond [(pair? val) (cdr val)]
            [else (error 'lookup-in-env "not found" name)]))))

;;; closure is structured as (formals body)
(define make-closure
  (lambda (formals body)
    (list 'closure formals body)))
(define formals-of
  (lambda (closure)
    (cadr closure)))
(define body-of
  (lambda (closure)
    (caddr closure)))

;;; apply in a closure
(define applying
  (lambda (closure vals env)
    (value-of (body-of closure)
              (extend-env (formals-of closure) vals env))))
;;; get values of a list
(define val-list
  (lambda (names env)
    (cond [(null? names) (quote ())]
          [else
           (cons (value-of (car names) env)
                 (val-list (cdr names) env))])))

(define value-of
  (lambda (e env)
    (pmatch `(,e)
      ; booleans and numbers
      [(,a) (guard (or (eq? a #f) (eq? a #t) (number? a)))
       a]
      ; variables
      [(,a) (guard (symbol? a))
       (lookup-in-env a env)]
      ; lambda
      [((lambda ,formals ,body))
       (make-closure formals body)]
      ; if
      [((if . ,args))
       (let [(condition (value-of (car args) env))]
         (cond [condition
                (value-of (cadr args) env)]
               [(not (null? (cddr args))) ;else part
                (value-of (caddr args) env)]))]
      [((let . ,args))
       (let [(binding-env (let-extend-env (car args) env))
             (body (cadr args))]
         (value-of body binding-env))]
      ; zero? sub1 *
      [((,func . ,args)) (guard (atom? func))
       (cond [(eq? func 'zero?)
              (zero? (value-of (car args) env))]
             [(eq? func 'sub1)
              (sub1 (value-of (car args) env))]
             [(eq? func '*)
              (* (value-of (car args) env)
                 (value-of (cadr args) env))]
             [else
              (error 'value-of "function not found" func)])]
      ; application
      [((,app . ,args))
       (let [(closure (value-of app env))
             (vals    (val-list args env))]
         (applying closure vals env))]
      [else
       (error 'value-of "unkown" e)])))

