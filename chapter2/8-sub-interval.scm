(define (sub-interval x y)
  (make-interval (- (lower-bound x)
                    (upper-bound y))
                 (- (upper-bound x)
                    (lower-bound y))))

;;; tests begin
(load "../testframe.scm")
(load "7-interval-selector.scm")

(let* ((i1 (make-interval 10 20))
       (i2 (make-interval 5 6))
       (result (sub-interval i1 i2)))
  (assertequal? (lower-bound result) (- 10 6))
  (assertequal? (upper-bound result) (- 20 5)))
