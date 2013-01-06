#lang plai-typed

;;; add variable(varC) to interpreter
;;; add set and seq to interpreter.

(define-type ExprC
  [numC (n : number)]
  [plusC (l : ExprC) (r : ExprC)]
  [appC (name : ExprC) (arg : ExprC)]
  [lamC (arg : symbol) (body : ExprC)]  
  [varC (var : symbol)]
  [setC (var : symbol) (val : ExprC)]
  [letC (name : symbol) (e : ExprC) (body : ExprC)]
  [seqC (b1 : ExprC) (b2 : ExprC)])

(define-type CVal 
  [numV (n : number)]
  [closureV (arg : symbol) (body : ExprC) (env : Env)]
  )

(define-type-alias Location number)
(define-type-alias Env (hashof symbol Location))
(define-type-alias Store (hashof Location CVal))
(define-type Result
  [v*s (v : CVal) (s : Store)])

(define (extend-env env s loc)
  (hash-set env s loc))

(define (extend-store store loc v) 
  (hash-set store loc v))

(define new-loc
  (let ((n 0))
    (lambda ()
      (begin (set! n (+ n 1))
             n))))
;;;;;;;;;;;;;;;;;;;; interpreter helper function

(define (lookup [k : symbol] [env : Env]) : Location
  (type-case (optionof 'a) (hash-ref env k)
    (some (v) v)
    (none () (error 'lookup "identifier not found"))))

(define (fetch [l : Location] [sto : Store]) : CVal
  (type-case (optionof 'a) (hash-ref sto l)
    (some (v) v)
    (none () (error 'lookup "identifier not found"))))

(define (num+ [l : CVal] [r : CVal]) : CVal
  (cond ((and (numV? l) (numV? r))
         (numV (+ (numV-n l) (numV-n r))))
        (else (error 'interp-env "type error."))))

;;;;;;;;;;;;;;;;;;;; interp-env

(define (interp-env [a : ExprC] [env : Env] [store : Store]) : Result
  (type-case ExprC a
    [numC (n) (v*s (numV n) store)]
    [plusC (l r)
           (type-case Result (interp-env l env store)
             (v*s (v-l s-l)
                  (type-case Result (interp-env r env s-l)
                    (v*s (v-r s-r)
                         (v*s (num+ v-l v-r) s-r)))))]
    [varC (var)
         (v*s (fetch (lookup var env) store) store)]
    ;;; NOTE: closure only stores the location of a symbol
    ;;; what if the location didn't change but store changed
    ;;; (let ((x 1))
    ;;;   (let ((f (lambda (y) x)))
    ;;;     (let ((x 2))
    ;;;       (f 10))))
    ;;; this is about dynamic scope and static scope again :P
    [lamC (arg body)
         (v*s (closureV arg body env) store)]
    [appC (name arg)
          (type-case Result (interp-env name env store)
            (v*s (v-lam s-lam) ; TODO: check closure type
                 (type-case Result (interp-env arg env s-lam)
                   (v*s (v-arg s-arg)
                        (let ((lam-arg (closureV-arg v-lam))
                              (lam-body (closureV-body v-lam))
                              (lam-env (closureV-env v-lam))
                              (loc (new-loc)))
                            (interp-env 
                             lam-body 
                             (extend-env lam-env lam-arg loc)
                             (extend-store store loc v-arg)))))))]
    ;;; after implementing setC, what's still missing?
    ;;; Yes! we can mutate variables in environment, but where can we create(or push)
    ;;; those variables in environment?
    ;;; maybe we need a `letC`, see below.
    [setC (var val)
          (type-case Result (interp-env val env store)
            (v*s (v s)
                 (let ((old-loc (lookup var env)))
                   (v*s v (extend-store s old-loc v)))))]
    [letC (name e body)
          (type-case Result (interp-env e env store)
            (v*s (v s)
                 (let ((loc (new-loc)))
                   (interp-env body
                               (extend-env env name loc)
                               (extend-store s loc v)))))]
    [seqC (b1 b2) 
          (type-case Result (interp-env b1 env store)
            (v*s (v s)
                 (interp-env b2 env s)))]
              
          ))

(define (interp (expr : ExprC)) : Result
  (interp-env expr (hash empty) (hash empty)))

;;; test, call-by-value, y is always 5
(interp (letC 'f (lamC 'x (setC 'x (numC 3)))
              (letC 'y (numC 5)
                    (seqC (appC (varC 'f) (varC 'y))
                          (varC 'y)))))
