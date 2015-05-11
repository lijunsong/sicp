(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

;;; iterative accumulate
(define (accumulate/iter combiner null-value term a next b)
  (define (iter c result)
    (if (> c b)
        result
        (iter (next c)
              (combiner result (term c)))))
  (iter a null-value))

(define (sum/iter term a next b)
  (accumulate/iter + 0 term a next b))

(define (product/iter term a next b)
  (accumulate/iter * 1 term a next b))

;;; tests begin
(load "../testframe.scm")

;; product tests
(let ((p (product (lambda (x) x)
                  1
                  (lambda (x) (+ 1 x))
                  10))
      (p1 (product/iter (lambda (x) x)
                        1
                        (lambda (x) (+ 1 x))
                        10)))
  (assert= p
           (* 2 3 4 5 6 7 8 9 10))
  (assert= p1
           (* 2 3 4 5 6 7 8 9 10)))

;; sum tests
; cube
(define (inc n) (+ n 1))
(define (cube x) (* x x x))
(define (sum-cubes a b)
  (sum cube a inc b))
(define (sum-cubes/iter a b)
  (sum/iter cube a inc b))
(assert= (sum-cubes 1 10) 3025)
(assert= (sum-cubes/iter 1 10) 3025)

; sum-integers
(define (identity x) x)
(define (sum-integers a b)
  (sum identity a inc b))
(define (sum-integers/iter a b)
  (sum/iter identity a inc b))
(assert= (sum-integers 1 10) 55)
(assert= (sum-integers/iter 1 10) 55)
