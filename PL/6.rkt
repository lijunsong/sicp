#lang plai-typed
(define-type ArithS
  [numS (n : number)]
  [plusS (l : ArithS) (r : ArithS)]
  [multS (l : ArithS) (r : ArithS)]
  [idS (id : symbol)]
  [appS (name : symbol) (arg : ArithS)]
  [uminusS (e : ArithS)]
  [minusS (l : ArithS) (r : ArithS)])

(define-type FuncDef
  [fdC (name : symbol) (arg : symbol) (body : ExprC)])

(define-type ExprC
  [numC (n : number)]
  [plusC (l : ExprC) (r : ExprC)]
  [appC (name : symbol) (arg : ExprC)]
  [idC (id : symbol)]
  [multC (l : ExprC) (r : ExprC)])

(define-type-alias Env (hashof symbol number))

(define-type-alias CVal number)

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

;;; find function in function definition list
(define (get-func [name : symbol] [fd : (listof FuncDef)]) : FuncDef
  (cond [(empty? fd) (error 'interp "not found function")]
        [(eq? name (fdC-name (first fd)))
         (first fd)]
        [else (get-func name (rest fd))]))

(define (lookup [k : symbol] [env : Env]) : CVal
  (type-case (optionof 'a) (hash-ref env k)
    (some (v) v)
    (none () (error 'lookup "identifier not found"))))
  
(define (interp [a : ExprC] [env : Env] [fd : (listof FuncDef)]) : CVal
  (type-case ExprC a
    [numC (n) n]
    [plusC (l r)
           (+ (interp l env fd) (interp r env fd))]
    [multC (l r)
           (* (interp l env fd) (interp r env fd))]
    [idC (id)
         (lookup id env)]
    [appC (name arg)
          (let* ((arg-val (interp arg env fd))
                 (func (get-func name fd))
                 (func-arg (fdC-arg func))
                 (func-body (fdC-body func)))
            (interp func-body
                    (hash-set (hash empty) func-arg arg-val) 
                    fd))]))

;;; spot the bug
(interp (desugar (parse '(double 3))) 
          (hash empty)
          (list (fdC 'double 'x (appC 'double-1 (idC 'x)))
                (fdC 'double-1 'y (plusC (idC 'x) (idC 'y)))))