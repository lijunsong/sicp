#lang plai-typed

(require (typed-in racket/pretty (pretty-print : ('a -> 'b))))
;;; a small type checker based on previous one
;;; 
;;; add recursion
;;; recC

(define-type Type
  [numC (n : number)]
  [plusC (l : Type) (r : Type)]
  [appC (name : Type) (arg : Type)]
  [lamC (arg : symbol)
        (argT : ResultType)
        (body : Type)
        (retT : ResultType)]  
  [idC (id : symbol)]
  [ifC (tst : Type) (thn : Type) (els : Type)] ; use numC as tst
  [recC (fun : symbol)
        (retT : ResultType)
        (arg : symbol)
        (argT : ResultType)
        (body : Type)
        (usage : Type)]
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
    [ifC (tst thn els)
         (local ((define tst-t (type-check tst env))
                 (define thn-t (type-check thn env))
                 (define els-t (type-check els env)))
           (cond [(not (numT? tst-t))
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
                  (else (error 'type-check "apply non-function"))))]
    [recC (fun retT arg argT body usage)
          (local ((define env0 (extend-env env fun (funcT argT retT)))
                  (define new-env (extend-env env0 arg argT))
                  (define ret-type (type-check body new-env)))
                 (cond [(not (equal? ret-type retT))
                        (error 'type-check "rec error")]
                       [else
                        (type-check usage env0)]))]
    ))

;; (rec (E num (n num)
;;         (if0 n 0 (+ n (E (+ -1 n))))
;;    (E 10)))
(type-check
 (recC 'E 
       (numT) ; return type
       'n ;argument
       (numT) ;argument type
       ; body
       (ifC (idC 'n) ;if0
            (numC 0)
            (plusC (idC 'n)
                   (appC (idC 'E) (plusC (numC -1) (idC 'n)))))
       ;usage
       (appC (idC 'E) (numC 10)))
 (hash empty))   
