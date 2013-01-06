#lang plai-typed

(require (typed-in racket/base (error : ('a 'b 'c -> number))))

(define-type ExprC
  [numC (n : number)]
  [plusC (l : ExprC) (r : ExprC)]
  [idC (id : symbol)]
  [letC (name : symbol) (e : ExprC) (body : ExprC)]
  [setC (name : symbol) (e : ExprC)]
  [seqC (e1 : ExprC) (e2 : ExprC)]
  )

;;; we are going to pass env in result.
(define-type Result
  (result [v : number] [env : Env]))

(define-type-alias Env (hashof symbol number))

(define (lookup [k : symbol] [env : Env]) : number
  (type-case (optionof 'a) (hash-ref env k)
    (some (v) v)
    (none () (error 'lookup "identifier not found: ~a" k))))

;;; interp : ExprC * Env -> number * Env
(define (interp [a : ExprC] [env : Env]) : Result
  (type-case ExprC a
    [numC (n) (result n env)]
    ;;; passing the environment from the left of addition to
    ;;; the right to reflect mutation
    [plusC (l r)
           (type-case Result (interp l env)
             (result (v-l env-l)
                     (type-case Result (interp r env-l)
                       (result (v-r env-r)
                               (result (+ v-l v-r) env-r)))))]
    [idC (id) (begin (display env)
         (result (lookup id env) env))]
    [letC (name e body)
          (type-case Result (interp e env)
            (result (v-e e-e)
                    (let ((new-env (hash-set e-e name v-e)))
                      (type-case Result (interp body new-env)
                        (result (v-body e-body) 
                                ;;NOTE: return env to avoid broken scope
                                (result v-body env))))))]
    [setC (name e)
          (type-case Result (interp e env)
            (result (v-e e-e)
                    (let ((new-env (hash-set e-e name v-e)))
                      (result v-e new-env))))]
    [seqC (e1 e2)
          (type-case Result (interp e1 env)
            (result (v-e1 e-e1)
                    (interp e2 e-e1)))]
                    
          ))

;;; passing environment from the left to right doesn't make anything messy.
;(let ((b 1))
;  (+ (begin (set! b 3) 23)
;     b))
(interp (letC 'b (numC 1)
              (plusC (seqC (setC 'b (numC 3))
                           (numC 23))
                     (idC 'b)))
        (hash empty))

;;; this will not break the scope, as well.
;(+ (let ((b 0)) 1)
;   b)
(interp (plusC (letC 'b (numC 0)
                     (numC 1))
               (idC 'b))
        (hash empty))