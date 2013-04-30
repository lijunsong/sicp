(define (sum-of-square-of-largest-two-1 x y z)
  (cond ((and (> x y) (> y z))
         (+ (* x x) (* y y)))
        ((and (> x y))
         (+ (* x x) (* z z)))
        ((> x z) ; and also (< x y)
         (+ (* x x) (* y y)))
        (else
         (+ (* z z) (* y y)))))


;;; tests begin
(load "../testframe.scm")

(assert= (sum-of-square-of-largest-two-1 2 3 4) 25)
(assert= (sum-of-square-of-largest-two-1 2 4 3) 25)
(assert= (sum-of-square-of-largest-two-1 3 2 4) 25)
(assert= (sum-of-square-of-largest-two-1 3 4 2) 25)
(assert= (sum-of-square-of-largest-two-1 4 2 3) 25)
(assert= (sum-of-square-of-largest-two-1 4 3 2) 25)
