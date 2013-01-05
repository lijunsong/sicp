#lang plai-typed

;;; use Racket lambda to implement lamC

(define-type ExprC
  [numC (n : number)]
  [plusC (l : ExprC) (r : ExprC)]
  [appC (name : ExprC) (arg : ExprC)]
  [lamC (arg : symbol) (body : ExprC)]  
  [idC (id : symbol)]
  )

(define-type CVal 
  [numV (n : number)]
  [closureV (f : ('a -> 'b))]
  )

(define (extend-env e s v)
  (hash-set e s v))

(define-type-alias Env (hashof symbol CVal))

(define (lookup (s : symbol) (env : Env)) : CVal
  (type-case (optionof 'a) (hash-ref env s)
    (some (v) v)
    (none () (error 'lookup "not found"))))

(define (interp (expr : ExprC) (env : Env)) : CVal
  (type-case ExprC expr
    (numC (n) (numV n))
    (idC (id) (lookup id env))
    (plusC (l r) 
           (let ((v-l (interp l env))
                 (v-r (interp r env)))
             (if (and (numV? v-l) (numV? v-r))
                 (numV (+ (numV-n v-l) (numV-n v-r)))
                 (error 'interp "type doesn't match"))))
    (lamC (arg body)
          (closureV
           (lambda (arg-val)
            (interp body (extend-env env arg arg-val)))))
    (appC (name arg)
          (let ((f (interp name env))
                (a (interp arg env)))
            ((closureV-f f) a)))))
            
;; test
(interp (appC (lamC 'x (plusC (numC 23) (idC 'x)))
              (numC 1))
        (hash empty))