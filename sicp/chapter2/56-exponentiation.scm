(load "differentiation.scm")

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (let ((e-base (base exp))
               (e-expo (exponent exp)))
           (make-product e-expo
                         (make-product (make-exponentiation e-base
                                                            (make-sum e-expo -1))
                                       (deriv e-base var)))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        (else
         (list '** b e))))

(define (exponentiation? e)
  (and (pair? e) (eq? '** (car e))))

(define (base e)
  (cadr e))

(define (exponent e)
  (caddr e))

;;; tests begin
;; serve as regression tests

(load "../testframe.scm")

; deriv should simplify the result
(assert= (deriv '(** x 1) 'x) 1)
(assert= (deriv '(** y 3) 'x) 0)

; deriv should be able to handle simple exponentiation
(assertequal? (deriv '(** x 2) 'x) '(* 2 x))
(assertequal? (deriv '(** x 3) 'x) '(* 3 (** x 2)))

; complex one
(assertequal? (deriv '(** (+ x 1) y) 'x) '(* y (** (+ x 1) (+ y -1))))
