#lang plai-typed
(define-type ArithS
  [numS (n : number)]
  [plusS (l : ArithS) (r : ArithS)]
  [multS (l : ArithS) (r : ArithS)]
  [idS (id : symbol)]
  [appS (name : symbol) (arg : ArithS)]
  [uminusS (e : ArithS)]
  [minusS (l : ArithS) (r : ArithS)])

(define-type ExprC
  [numC (n : number)]
  [plusC (l : ExprC) (r : ExprC)]
  [appC (name : symbol) (arg : ExprC)]
  [fdC (arg : symbol) (body : ExprC)]  
  [idC (id : symbol)]
  [multC (l : ExprC) (r : ExprC)])

(define-type CVal 
  [numV (n : number)]
  [funcV (arg : symbol) (body : ExprC)]
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
          (appS (s-exp->symbol (first lst))
                (parse (second lst)))]))]
    [else (error 'parse "invalid outpur")]))

(define (desugar [s : ArithS]) : ExprC
  (type-case ArithS s
    [numS (n) (numC n)]
    [idS (id) (idC id)]
    [appS (name arg)
          (appC name (desugar arg))]
    [plusS (l r) (plusC (desugar l) (desugar r))]
    [multS (l r) (multC (desugar l) (desugar r))]
    [uminusS (e) (multC (numC -1) (desugar e))]
    [minusS (l r) (plusC (desugar l) 
                         (desugar (uminusS r)))]))

(define (lookup [k : symbol] [env : Env]) : CVal
  (type-case (optionof 'a) (hash-ref env k)
    (some (v) v)
    (none () (error 'lookup "identifier not found"))))

;;; problem
(define (interp [a : ExprC] [env : Env]) : CVal
  (type-case ExprC a
    [numC (n) n]
    [plusC (l r)
           (+ (interp l env) (interp r env))]
    [multC (l r)
           (* (interp l env) (interp r env))]
    [idC (id)
         (lookup id env)]
    [fdC (arg body)
         (error 'interp "you cannot interp fdC directly")]
    [appC (name arg)
          (let* ((arg-val (interp arg env))
                 (func (lookup name env)) ;should be fdC
                 (func-arg (fdC-arg func))
                 (func-body (fdC-body func)))
            (interp func-body
                    (hash-set env func-arg arg-val)))]))

;;; test
;(interp (desugar (parse '(double 3))) 
;          (hash empty)
;          (list (fdC 'double 'x (multC (numC 2) (idC 
;                                                 'x)))))