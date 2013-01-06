#lang plai-typed

;;; a small type checker, based on the model of interpreter
;;;
;;; This type checker has a critical problem! refer to the definition of closureT
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
  (closureT (arg : symbol)   ;;;closureT has a critical problem, see the bottom of this file.
            (argT : ResultType)
            (body : Type)
            (retT : ResultType)
            (env : Env)))

(define-type-alias Env (hashof symbol ResultType))
(define (extend-env (env : Env) (s : symbol) (v : ResultType)) : Env
  (hash-set env s v))

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
    ;;; check 3 things:
    ; 1. name is closureT
    ; 2. arg's type is consistant with that of the argument of closure
    ; 3. closure body is retT
    [appC (name arg)
          (let ((f (type-checker name env)))
            (if (not (closureT? f))
                (error 'type-checker "apply non-function")   ;1
                (let ((a-t (type-checker arg env))
                      (fa-t (closureT-argT f)))
                  (if (not (equal? a-t fa-t))
                      (error 'type-checker "mismatch argument type") ;2
                      (let* ((a (closureT-arg f))
                             (new-env (extend-env env a (closureT-argT f)))
                             (body-t (type-checker (closureT-body f) new-env)))
                        (if (not (equal? body-t (closureT-retT f)))
                            (error 'type-checker "function return mismatch") ;3
                            body-t))))))]))
                        
                
                     
(type-checker
 (appC
  (appC
   (lamC 'x
         (numT)
         (lamC 'y 
               (numT)
               (plusC (idC 'y) (idC 'x))
               (numT))
         ;;;PROBLEM: it is impossible to annotate function return type here
         ;;; closureT needs env to be complete!
         (closureT 'y (numT) (plusC (idC 'y) (idC 'x)) (numT)))  
   (numC 2))
  (numC 3))
 (hash empty))