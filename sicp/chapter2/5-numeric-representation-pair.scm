;;; cons integers

(define (cons x y)
  (* (expt 2 x) (expt 3 y)))

;; nth root of num = result => get n
(define (nth-root num result)
  (if (not (zero? (remainder num result)))
      0
      (1+ (nth-root (/ num result) result))))

;(nth-root 1024 2) => 10

(define (car z)
  (nth-root z 2))

(define (cdr z)
  (nth-root z 3))
