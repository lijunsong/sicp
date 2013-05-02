(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (+ (term a) result))))
  (iter a 0))

;;; tests begin
(load "../testframe.scm")

; cube
(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))
(assert= (sum-cubes 1 10) 3025)

; sum-integers
(define (identity x) x)
(define (sum-integers a b)
  (sum identity a inc b))
(assert= (sum-integers 1 10) 55)
