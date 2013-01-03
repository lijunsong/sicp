#lang plai-typed
(define-type ArithS
  [numS (n : number)]
  [plusS (l : ArithS) (r : ArithS)]
  [multS (l : ArithS) (r : ArithS)]
  [idS (id : symbol)]
  [appS (name : ArithS) (arg : ArithS)]
  [uminusS (e : ArithS)]
  [minusS (l : ArithS) (r : ArithS)])

(define-type ExprC
  [numC (n : number)]
  [plusC (l : ExprC) (r : ExprC)]
  [appC (name : ExprC) (arg : ExprC)]
  [lamC (arg : symbol) (body : ExprC)]  
  [idC (id : symbol)]
  [multC (l : ExprC) (r : ExprC)])

(define-type CVal 
  [numV (n : number)]
  [closureV (arg : symbol) (body : ExprC) (env : Env)]
  )

(define-type-alias Env (hashof symbol CVal))

(define (parse [s : s-expression]) : ArithS
  (cond 
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-symbol? s) (idS (s-exp->symbol s))]
    [(s-exp-list? s)
     (let ((lst (s-exp->list s)))
       (case (s-exp->symbol (first lst))
         [(+) (plusS (parse (second lst)) (parse (third lst)))]
         [(*) (multS (parse (second lst)) (parse (third lst)))]
         [(-) (cond [(empty? (rest (rest lst)))
                     (uminusS (parse (second lst)))]
                    [else
                     (minusS (parse (second lst)) (parse (third lst)))])]
         [else 
          (appS (idS (s-exp->symbol (first lst)))
                (parse (second lst)))]))]
    [else (error 'parse "invalid outpur")]))

(define (desugar [s : ArithS]) : ExprC
  (type-case ArithS s
    [numS (n) (numC n)]
    [idS (id) (idC id)]
    [appS (name arg)
          (appC (desugar name) (desugar arg))]
    [plusS (l r) (plusC (desugar l) (desugar r))]
    [multS (l r) (multC (desugar l) (desugar r))]
    [uminusS (e) (multC (numC -1) (desugar e))]
    [minusS (l r) (plusC (desugar l) 
                         (desugar (uminusS r)))]))

(define (lookup [k : symbol] [env : Env]) : CVal
  (type-case (optionof 'a) (hash-ref env k)
    (some (v) v)
    (none () (error 'lookup "identifier not found"))))

(define (num+ [l : CVal] [r : CVal]) : CVal
  (cond ((and (numV? l) (numV? r))
         (numV (+ (numV-n l) (numV-n r))))
        (else (error 'interp "type error."))))

(define (num* [l : CVal] [r : CVal]) : CVal
  (cond ((and (numV? l) (numV? r))
         (numV (* (numV-n l) (numV-n r))))
        (else (error 'interp "type error."))))

;;; 
(define (interp [a : ExprC] [env : Env]) : CVal
  (type-case ExprC a
    [numC (n) (numV n)]
    [plusC (l r)
           (num+ (interp l env) (interp r env))]
    [multC (l r)
           (num* (interp l env) (interp r env))]
    [idC (id)
         (lookup id env)]
    [lamC (arg body)
         (closureV arg body env)]
    [appC (name arg)
          (let* ((arg-val (interp arg env))
                 (lam (interp name env))
                 (lam-arg (closureV-arg lam))
                 (lam-body (closureV-body lam)))
            (interp lam-body
                    (hash-set (closureV-env lam) lam-arg arg-val)))]))

;;; fix test broken
(interp (appC
         (appC
          (lamC 'x (lamC 'y (plusC (idC 'y) (idC 'x))))
          (numC 2))
         (numC 3))
        (hash empty))