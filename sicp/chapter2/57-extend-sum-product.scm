(load "differentiation.scm")

(define (augend s)
  (if (> (length s) 3)
      (cons '+ (cddr s))
      (caddr s)))

(define (multiplicand m)
  (if (> (length m) 3)
      (cons'* (cddr m))
      (caddr m)))


;;; tests begin
(load "../testframe.scm")

;; regression tests
(assert= (deriv '(+ x 3) 'x) 1)

(asserteq? (deriv '(* x y) 'x) 'y)

(assertequal? (deriv '(* (* x y) (+ x 3)) 'x)
              '(+ (* x y) (* y (+ x 3))))

;; new tests
(assertequal? (deriv '(* x y (+ x 3)) 'x)
              '(+ (* x y) (* y (+ x 3))))

(assertequal? (deriv '(+ x (* x 1) x x x) 'x) 5)

;(assertequal? (deriv '(* x x x x) 'x)
;              (deriv '(* x (* x (* x x))) 'x))

