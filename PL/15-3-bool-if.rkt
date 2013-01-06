#lang plai-typed

;;; a small type checker based on previous on
;;; 
;;; add boolC and ifC

(define-type Type
  [numC (n : number)]
  [plusC (l : Type) (r : Type)]
  [appC (name : Type) (arg : Type)]
  [lamC (arg : symbol)
        (argT : ResultType)
        (body : Type)
        (retT : ResultType)]  
  [idC (id : symbol)]
  [boolC (b : number)] ; This can be done by directly using numC
  [ifC (tst : Type) (thn : Type) (els : Type)]
  [multC (l : Type) (r : Type)])

(define-type ResultType
  (numT)
  (boolT)
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
    [boolC (b) (boolT)]
    [plusC (l r)
           (all-numT? l r env)]
    [multC (l r)
           (all-numT? l r env)]
    [ifC (tst thn els)
         (local ((define tst-t (type-check tst env))
                 (define thn-t (type-check thn env))
                 (define els-t (type-check els env)))
           (cond [(not (boolT? tst-t))
                  (error 'type-check "test in condition is not bool")]
                 [(not (equal? thn-t els-t))
                  (error 'type-check "the type of if branches don't match")]
                 [else
                  thn-t]))]
           
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
 (ifC (boolC 0)
      (plusC (appC (lamC 'x (numT)
                         (numC 23) (numT))
                   (numC 1))
             (numC 23))
      (numC 0))
 (hash empty))