(define (fast-expt-iter b n)
  (define (fast-expt-iter-helper b n a)
    (cond ((= n 0) a)
          ((even? n) (fast-expt-iter-helper (* b b) (/ n 2) a))
          (else
           (fast-expt-iter-helper b (- n 1) (* a b)))))
  (fast-expt-iter-helper b n 1))

; for testing purpose
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0))

;;; tests begin
(load "../testframe.scm")

(assert= (fast-expt-iter 2 11)
         (fast-expt 2 11))

(assert= (fast-expt-iter 4 19)
         (fast-expt 4 19))
