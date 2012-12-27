#lang plai-typed
(define-type ArithS
  [numS (n : number)]
  [plusS (l : ArithS) (r : ArithS)]
  [multS (l : ArithS) (r : ArithS)]
  [uminusS (e : ArithS)]
  [minusS (l : ArithS) (r : ArithS)])

(define-type ArithC
  [numC (n : number)]
  [plusC (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)])

(define (parse [s : s-expression]) : ArithS
  (cond 
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-list? s)
     (let ((lst (s-exp->list s)))
       (case (s-exp->symbol (first lst))
         [(+) (plusS (parse (second lst)) (parse (third lst)))]
         [(*) (multS (parse (second lst)) (parse (third lst)))]
         [(-) (cond [(empty? (rest (rest lst)))
                     (uminusS (parse (second lst)))]
                    [else
                     (minusS (parse (second lst)) (parse (third lst)))])]
         [else (error 'parse "unrecognized symbol")]))]
    [else (error 'parse "invalid outpur")]))

(define (desugar [s : ArithS]) : ArithC
  (type-case ArithS s
    [numS (n) (numC n)]
    [plusS (l r) (plusC (desugar l) (desugar r))]
    [multS (l r) (multC (desugar l) (desugar r))]
    [uminusS (e) (multC (numC -1) (desugar e))]
    [minusS (l r) (plusC (desugar l) 
                         (desugar (uminusS r)))]))
    
    
(define (interp [a : ArithC]) : number
  (type-case ArithC a
    [numC (n) n]
    [plusC (l r)
           (+ (interp l) (interp r))]
    [multC (l r)
           (* (interp l) (interp r))]))