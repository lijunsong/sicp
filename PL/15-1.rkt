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
  (closureT (arg : symbol)
            (argT : ResultType)
            (body : Type)
            (retT : ResultType)
            (env : Env)))

(define-type-alias Env (hashof symbol ResultType))

(define (lookup [k : symbol] [env : Env]) : ResultType
  (type-case (optionof 'a) (hash-ref env k)
    (some (v) v)
    (none () (error 'lookup "identifier not found"))))

(define (all-numT? (e1 : Type) (e2 : Type) (env : Env)) : ResultType
  (if (and (numT? (type-checker e1 env))
           (numT? (type-checker e2 env)))
      (numT)
      (error 'type-checker "not all are numbers")))

;;; type checker
(define (type-checker [a : Type] [env : Env]) : ResultType
  (type-case Type a
    [numC (n) (numT)]
    [plusC (l r)
           (all-numT? l r env)]
    [multC (l r)
           (all-numT? l r env)]
    [idC (id)
         (lookup id env)]
    [lamC (arg argT body retT)
         (closureT arg argT body retT env)]
    ;;; problematic 
    [appC (name arg)
          (let* ((arg-type (type-checker arg env))
                 (closure (type-checker name env))
                 (closure-body (closureT-body closure))
                 (closure-arg (closure
                 (closure-body-type (type-checker closure-body env))
                 (closure-arg-type (closureT-argT closure))
                 (closure-ret-type (closureT-retT closure)))
            ;;type-checker body against with declared type
            ;;type-checker closure arg against with given arg
            (cond ((not (equal? closure-ret-type closure-body-type))
                   (error 'type-checker "body error"))
                  ((not (equal? arg-type closure-arg-type))
                   (error 'type-checker "arg error"))
                  (else
                   closure-ret-type)))]))
                     
(type-checker
 (appC
  (appC
   (lamC 'x
         (numT)
         (lamC 'y 
               (numT)
               (plusC (idC 'y) (idC 'x))
               (numT))
         (numT))
   (numC 2))
  (numC 3))
 (hash empty))