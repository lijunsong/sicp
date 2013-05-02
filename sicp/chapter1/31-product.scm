(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

;; product tests begin
(load "../testframe.scm")
(assert= (product (lambda (x) x)
                  1
                  (lambda (x) (+ 1 x))
                  10)
         (* 2 3 4 5 6 7 8 9 10))

;;; 1. factorial
(define (factorial n)
  (define (identity x) x)
  (define (inc x) (+ 1 x))
  (product identity 1 inc n))

;; 1. tests begin

(assert= (factorial 1) 1)
(assert= (factorial 4) 24)

;;; 2. get pi from the wallis-product
(define (wallis-product n)
  (define (f item)
    (cond ((even? item) (/ item (+ item 1)))
          (else (/ (+ item 1) item))))
  (define (inc x)
    (+ x 1))
  (* 2.0 (product f 1 inc n)))

;;; spot the result
(wallis-product 10) ;Value: 3.002175954556907
(wallis-product 100) ;Value: 3.1260789002154112
(wallis-product 1000) ;Value: 3.1400238186005973
(wallis-product 10000) ;Value: 3.1414355935899083

;;; 3. write iterative product
(define (product/iter item a next b)
  (define (iter c result)
    (if (> c b)
        result
        (iter (next c)
              (* result (item c)))))
  (iter a 1))


(assert= (factorial/testing 4) 24)

