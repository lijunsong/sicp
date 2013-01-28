#lang racket

;;; cons-all will cons the all arguments one by one
;;; (cons-all 1 2 3) will transform to (cons 1 (cons 2 (list 3)))
(define-syntax cons-all
  (syntax-rules ()
    ((cons-all a)
     (list a))
    ((cons-all a0 a ...)
     (cons a0 (cons-all a ...)))))

;; test
(cons-all 1 2 3 4) ;=> produce '(1 2 3 4)


;;;use syntax-case to define syntax-rules

;; the original syntax-rules's usage
;; ((syntax-rules ()
;;     [(my-let ([var val] ...) body)
;;      ((lambda (var ...) body) val ...)])
;;  #'(my-let ([x 1]
;;             [y 2])
;;            (+ x y)))

;; the original syntax-case's usage
;; ((lambda (stx)
;;    (syntax-case stx ()
;;      [(my-let ([var val] ...) body)
;;       #'((lambda (var ...) body) val ...)]))
;;  #'(my-let ([x 1]
;;             [y 2])
;;            (+ x y)))

(define-syntax (my-syntax-rules stx)
  (syntax-case stx ()
    ((my-syntax-rules
      (literal-id ...) ((id pattern ...) template) ...)
     #'(lambda (s)
         (syntax-case s (literal-id ...)
           ((id pattern ...) #'template) ...)))))

;; test with one rule
((my-syntax-rules ()
    [(my-let ([var val] ...) body)
     ((lambda (var ...) body) val ...)])
 #'(my-let ([x 1]
            [y 2])
           (+ x y)))

;; test with two rules
(syntax->datum
 ((my-syntax-rules ()
                   [(my-let ([var val]) body)
                    ((lambda (var) body) val)]                  
                   [(my-let ([var0 val0] [var1 val1] ...) body)
                    ((lambda (var0)
                       (my-let ([var1 val1] ...) body))
                     val0)])
  #'(my-let ([x 1]
             [y 2])
            (+ x y))))

;; let with guard
(define-syntax (my-let-1 stx)
  (syntax-case stx ()
    [(my-let-1 [var val] body)
     (identifier? #'var)
     #'((lambda (var) body) val)]))

(my-let-1
 (x 2)
 (+ 2 3))
