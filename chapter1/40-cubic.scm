(load "fixed-point.scm")

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

;; define cubic
(define (cubic a b c)
  (lambda (x)
    (+ (* x x x) (* a x x) (* b x) c)))

;; for tests
(define (get-cubic-zeros a b c)
  (newtons-method (cubic a b c) 1))

; (get-cubic-zeros -3 3 -1) or (x-1)^3=0
; => 1

; (get-cubic-zeros 0 -3 2) or (x+2)(x-1)^2=0
; => 1

; (get-cubic-zeros -0.5 -4 -2.5) or (x-2.5)(x+1)^2=0
; => -1.0000050459462515

; (get-cubic-zeros 11/12 1/4 1/48) or (x+1/2)(x+1/4)(x+1/6)
; => -.16666666563804614
