(define (make-interval a b)
  (cons a b))

(define (upper-bound z)
  (max (car z) (cdr z)))

(define (lower-bound z)
  (min (car z) (cdr z)))

;;; tests begin
(load "../testframe.scm")

(let ((interval1 (make-interval -1 2))
      (interval2 (make-interval 3 2)))
  (begin
    (assert= -1 (lower-bound interval1))
    (assert= 2  (upper-bound interval1))
    (assert= 2  (lower-bound interval2))
    (assert= 3  (upper-bound interval2))))
