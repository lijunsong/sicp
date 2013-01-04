#lang plai-typed

(require (typed-in racket/base (error : ('a 'b 'c -> number))))

(define-type ExprC
  [numC (n : number)]
  [plusC (l : ExprC) (r : ExprC)]
  [idC (id : symbol)]
  [letC (name : symbol) (e : ExprC) (body : ExprC)]
  )

(define-type Result
  (result [v : number] [env : Env]))
(define-type-alias Env (hashof symbol number))

(define (lookup [k : symbol] [env : Env]) : number
  (type-case (optionof 'a) (hash-ref env k)
    (some (v) v)
    (none () (error 'lookup "identifier not found: ~a" k))))
;;; 
(define (interp [a : ExprC] [env : Env]) : Result
  (type-case ExprC a
    [numC (n) (result n env)]
    [plusC (l r)
           (type-case Result (interp l env)
             (result (v-l env-l)
                     (type-case Result (interp r env-l)
                       (result (v-r env-r)
                               (result (+ v-l v-r) env-r)))))]
    [idC (id)
         (result (lookup id env) env)]
    [letC (name e body)
          (type-case Result (interp e env)
            (result (v-e e-e)
                    (let ((new-env (hash-set e-e name v-e)))
                      (type-case Result (interp body new-env)
                        (result (v-body e-body) 
                                ;;NOTE: return env to avoid broken scope
                                (result v-body env))))))]
          ))

;;; this won't break!
(interp (plusC (letC 'b (numC 1)
                     (numC 3))
               (idC 'b))
        (hash empty))