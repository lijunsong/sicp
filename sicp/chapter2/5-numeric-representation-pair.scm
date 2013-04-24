;;; cons integers

(define (new-cons x y)
  (* (expt 2 x) (expt 3 y)))

;; nth root of num = result => get n
(define (nth-root num result)
  (if (not (zero? (remainder num result)))
      0
      (1+ (nth-root (/ num result) result))))

(define (new-car z)
  (nth-root z 2))

(define (new-cdr z)
  (nth-root z 3))


;;; tests begin

(load "../testframe.scm")

;; test nth-root
(assert= (nth-root 1024 2) 10) ; 2^10 = 1024
(assert= (nth-root 27 3) 3) ; 3^3 = 27
(assert= (nth-root 625 5) 4) ; 5^4 = 625

(assert= (new-car (new-cons 10 101)) 10)
(assert= (new-cdr (new-cons 10 101)) 101)
