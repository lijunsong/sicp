(define (double x)
  (* x 2))
(define (halve x)
  (/ x 2))

;; recursion
(define (x/rec a b)
  (cond ((= b 0) 0)
        ((even? b) (double (x/rec a (/ b 2))))
        (else
         (+ a (x/rec a (- b 1))))))

;; iteration
(define (x a b)
  (define (x/iter aa bb ans)
    (cond ((= bb 0) ans)
          ((even? bb) (x/iter (double aa) (halve bb) ans))
          (else
           (x/iter aa (- bb 1) (+ ans aa)))))
  (x/iter a b 0))

;;; tests begin
(load "../testframe.scm")

(assert= (x 2 1) 2)
(assert= (x 2 11) 22)
(assert= (x 3 14) 42)

(assert= (x/rec 2 1) 2)
(assert= (x/rec 2 11) 22)
(assert= (x/rec 3 14) 42)
