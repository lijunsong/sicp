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
    
;;; substitute the answer for symbols in expression
(define (subst [what : number] [for : symbol] [in : ExprC]) : ExprC
  (type-case ExprC in
    [numC (n) in]
    [idC (id) (if (eq? id for) 
                  (numC what)
                  in)]
    [plusC (l r)
           (plusC (subst what for l) (subst what for r))]
    [multC (l r)
           (multC (subst what for l) (subst what for r))]
    [appC (name arg)
          (appC name (subst what for arg))]))
           
(define (interp [a : ExprC] [fd : (listof FuncDef)]) : number
  (type-case ExprC a
    [numC (n) n]
    [plusC (l r)
           (+ (interp l fd) (interp r fd))]
    [multC (l r)
           (* (interp l fd) (interp r fd))]
    [idC (id)
         (error 'interp "unbound id")]
    [appC (name arg)
          (let* ((arg-val (interp arg fd))
                 (func (get-func name fd))
                 (func-arg (fdC-arg func))
                 (func-body (fdC-body func)))
            (interp (subst arg-val func-arg func-body) fd))]))