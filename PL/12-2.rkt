#lang plai-typed

;;; use Racket lambda to implement lamC, AND
;;; then implement dynamic scope
;;;
;;;
;;; Oh NO, internal lambda is actually static scope
;(lamC (arg body)
;      (closureV
;       (lambda (arg-val)
;         (interp body (extend-env env arg arg-val)))))
;seems dynamic use env, but it isn't!
;the value of `env` `arg` in (lambda (arg-val)...) is all 
;stored in the environment of Racket, so it is static.

(define-type ExprC
  [numC (n : number)]
  [plusC (l : ExprC) (r : ExprC)]
  [appC (name : ExprC) (arg : ExprC)]
  [lamC (arg : symbol) (body : ExprC)]  
  [idC (id : symbol)]
  [letC (name : symbol) (e : ExprC) (body : ExprC)]
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
          (begin (display env)
                 (display "[1]\n")
                 (closureV
                  (lambda (arg-val)
                    (begin (display "entend-env ")
                           (display env)
                           (display " with ")
                           (display arg)
                           (display "  ")
                           (display arg-val)
                           (display "[2]\n")
                           (interp body (extend-env env arg arg-val)))))))
    (letC (name e body)
          (let ((v-e (interp e env)))
            (interp body (extend-env env name v-e))))
    (appC (name arg)
          (let ((f (interp name env))
                (a (interp arg env)))
            (begin (display env)
                   (display "[3]\n")
                   ((closureV-f f) a))))))
            
;; test
(interp (letC 'x (numC 1) 
              (letC 'f (lamC 't (idC 'x))
                    (letC 'x (numC 2)
                          (appC (idC 'f) (numC 23)))))
        (hash empty))