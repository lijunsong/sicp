(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

;;; tests begin
(load "../testframe.scm")

(assert= (horner-eval 2 (list 1 3 0 5 0 1))
         79)
