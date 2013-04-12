(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (x y) x)))

;; the corresponding definition of cdr:
(define (cdr z)
  (z (lambda (x y) y)))
