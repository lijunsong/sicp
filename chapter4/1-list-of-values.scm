;; left to right
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((v (eval (first-operand exps) env)))
        (cons v (list-of-values (rest-operands exps) env)))))

;; right to left
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((v-rest (list-of-values (rest-operands exps) env))
            (v (eval (first-operand exps) env)))
        (cons v v-rest))))

