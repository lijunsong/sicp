#lang plai-typed

(define-type ArithC
  [numC (n : number)]
  [plusC (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)])

(define (parse [s : s-expression]) : ArithC
  (cond 
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-list? s)
     (let ((lst (s-exp->list s)))
       (case (s-exp->symbol (first lst))
         [(+) (plusC (parse (second lst)) (parse (third lst)))]
         [(*) (multC (parse (second lst)) (parse (third lst)))]
         [else (error 'parse "unrecognized symbol")]))]
    [else (error 'parse "invalid outpur")]))