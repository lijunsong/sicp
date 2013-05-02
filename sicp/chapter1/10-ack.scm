(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))


(define (f n) (A 0 n))

(define (g n) (A 1 n))

(define (h n) (A 2 n))

(define (f1 n) (* 2 n))

(define (g1 n) (expt 2 n))

(define (h1 n)
  (cond ((= n 0) 1)
        (else
         (expt 2 (h1 (- n 1))))))
;;; tests begin
(load "../testframe.scm")

(for-each (lambda (n)
            (assert= (f n) (f1 n))
            (assert= (g n) (g1 n)))
          '(1 2 3 4 5 6 7 8 9 10))

(for-each (lambda (n)
            (assert= (h n) (h1 n)))
          '(1 2 3 4)) ; n should be < 5 for saving time on calculation
