(define (compose f g)
  (lambda (t)
    (f (g t))))

;;; tests begin

(load "../testframe.scm")

(assert= ((compose square (lambda (x) (+ x 1))) 6) 49)
