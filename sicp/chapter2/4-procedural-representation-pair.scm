(define (new-cons x y)
  (lambda (m) (m x y)))

(define (new-car z)
  (z (lambda (x y) x)))

;; the corresponding definition of cdr:
(define (new-cdr z)
  (z (lambda (x y) y)))

;;; tests begin
(load "../testframe.scm")

(asserteq? (new-car (new-cons 'a 'b)) 'a)
(asserteq? (new-cdr (new-cons 'a 'b)) 'b)
