(define (filtered-accumulate combiner null-value term a next b p)
  (cond ((> a b) null-value)
        ((not (p a))
         (filtered-accumulate combiner null-value term (next a) next b p))
        (else
         (combiner (term a)
                   (filtered-accumulate combiner null-value term (next a) next b p)))))

;; apply filtered-accumulate
(load "prime.scm")
(define (sum-of-square-of-prime a b)
  (define (inc x) (+ x 1))
  (filtered-accumulate + 0 square a inc b prime?))

(define (product-of-positive-and-prime n)
  (define (identity x) x)
  (define (inc x) (+ 1 x))
  (define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))
  (define (relative-prime-to-n? x)
    (= (gcd x n) 1))
  (filtered-accumulate * 1 identity 1 inc n relative-prime-to-n?))

;;; tests begin
(load "../testframe.scm")

(assert= (sum-of-square-of-prime 1 10)
         (+ (* 2 2) (* 3 3) (* 5 5) (* 7 7)))
(assert= (product-of-positive-and-prime 10)
         (* 3 7 9))
(assert= (product-of-positive-and-prime 11)
         (* 1 2 3 4 5 6 7 8 9 10))
