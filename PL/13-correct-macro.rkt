#lang racket

;;;; using macro to define or

;; error: not considering the empty expression
(define-syntax (my-or-1 x)
  (syntax-case x ()
    ((my-or-1 e0 e1 ...)
     #'(if e0 e0 (my-or-1 e1 ...)))))

; test
;(my-or-1 false false 3)


;; almost correct
(define-syntax (my-or-2 x)
  (syntax-case x ()
    ((my-or-2) #'false)
    ((my-or-2 e0 e1 ...)
     #'(if e0
           e0
           (my-or-2 e1 ...)))))

;; correct code

(define-syntax (my-or-3 x)
  (syntax-case x ()
    ((my-or-3) #'false)
    ((my-or-3 e0 e1 ...)
     #'(let ((v e0))
         (if v
             v
             (my-or-3 e1 ...))))))
  
;; test hygiene
(let ((v true))
  (my-or-3 false v))
;; will expand with hygiene to
;(let ((v true)) 
;  (let ((v false)) 
;    (if v 
;        v
;        (let ((v v))
;          (if v
;              v
;              false)))))
