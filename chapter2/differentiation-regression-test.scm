;;; this file is only for loading from the real implementation

(load "../testframe.scm")

; sum and product
(assert= (deriv '(+ x 3) 'x) 1)
(asserteq? (deriv '(* x y) 'x) 'y)

(assertequal? (deriv '(* (* x y) (+ x 3)) 'x)
              '(+ (* x y) (* y (+ x 3))))

; exponent
; deriv should simplify the result
(assert= (deriv '(** x 1) 'x) 1)
(assert= (deriv '(** y 3) 'x) 0)

; deriv should be able to handle simple exponentiation
(assertequal? (deriv '(** x 2) 'x) '(* 2 x))
(assertequal? (deriv '(** x 3) 'x) '(* 3 (** x 2)))

; complex one
(assertequal? (deriv '(** (+ x 1) y) 'x) '(* y (** (+ x 1) (+ y -1))))
