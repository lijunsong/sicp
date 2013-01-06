#lang plai-typed

(define-type Type
  [numC (n : number)]
  [plusC (l : Type) (r : Type)]
  [appC (name : Type) (arg : Type)]
  [lamC (arg : symbol)
        (argT : ResultType)
        (body : Type)
        (retT : ResultType)]  
  [idC (id : symbol)]
  [multC (l : Type) (r : Type)])

(define-type ResultType
  (numT)
  (funcT (argT : ResultType) (retT : ResultType)))
            

(define-type-alias EnvT (hashof symbol ResultType))
(define (extend-env (env : EnvT) (s : symbol) (v : ResultType)) : EnvT
  (hash-set env s v))
  

(define (lookup [k : symbol] [env : EnvT]) : ResultType
  (type-case (optionof 'a) (hash-ref env k)
    (some (v) v)
    (none () (error 'lookup "identifier not found"))))

(define (all-numT? (e1 : Type) (e2 : Type) (env : EnvT)) : ResultType
  (if (and (numT? (type-check e1 env))
           (numT? (type-check e2 env)))
      (numT)
      (error 'type-check "not all are numbers")))

;;; type checker
(define (type-check [a : Type] [env : EnvT]) : ResultType
  (type-case Type a
    [numC (n) (numT)]
    [plusC (l r)
           (all-numT? l r env)]
    [multC (l r)
           (all-numT? l r env)]
    [idC (id)
         (lookup id env)]
    [lamC (arg argT body retT)
          (local ((define new-env (extend-env env arg argT))
                  (define ret-type (type-check body new-env)))
            (if (equal? retT ret-type)
                (funcT argT retT)
                (error 'type-check "function error")))]
    ; type check:
    ; 1. function is on function position
    ; 2. argument meet the requirement of function formal argument
    ; 3. otherwise, return funcT
    [appC (name arg)
          (let ((f (type-check name env))
                (a (type-check arg env)))
            (cond ((and (funcT? f) (equal? (funcT-argT f) a))
                   (funcT-retT f))
                  ((funcT? f) (error 'type-check "function argument has error"))
                  (else (error 'type-check "apply non-function"))))]))
  
                     
(type-check
 (appC
  (appC
   (lamC 'x
         (numT)
         (lamC 'y 
               (numT)
               (plusC (idC 'y) (idC 'x))
               (numT))
         (funcT (numT) (numT))) ; this lambda actually returns funcT
   (numC 2))
  (numC 3))
 (hash empty))