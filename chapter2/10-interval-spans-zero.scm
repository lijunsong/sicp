(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

;;; check the condition that an interval spans zero in div
(define (div-interval x y)
  (cond ((and (< (lower-bound y) 0)
              (> (upper-bound y) 0))
         (error 'div-interval "divide by an interval that spans zero!"))
        (else
         (mul-interval x
                       (make-interval (/ 1.0 (upper-bound y))
                                      (/ 1.0 (lower-bound y)))))))

;;; tests begin
(load "../testframe.scm")
(load "7-interval-selector.scm")

(let ((x (make-interval 19 20))
      (y (make-interval -1 1))
      (z (make-interval 4 5)))
  (assert/exn (div-interval x y) "divide"))
