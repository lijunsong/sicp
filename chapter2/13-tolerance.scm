(load "12-make-center-percent.scm")

;; test 1
(define i1 (make-center-percent 6.8 0.1))
(define i2 (make-center-percent 4.7 0.05))


;; get the percent of the multiplication of the two intervals

(percent (mul-interval i1 i2))
;Value: .1492537313432837

;; test 2
(define i3 (make-center-percent 1023 0.04))
(define i4 (make-center-percent 10   0.04))
(percent (mul-interval i3 i4))
;Value: .07987220447284339

;; test 3
(define i5 (make-center-percent 1 2/1000))
(define i6 (make-center-percent 1 2/1000))
(percent (mul-interval i5 i6))
;Value: 1000/250001
