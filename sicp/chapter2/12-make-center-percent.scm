(load "7-interval-selector.scm")
(load "10-interval-spans-zero.scm")

(define (make-center-percent c p)
  (make-interval (- c (* c p))
                 (+ c (* c p))))

(define (percent i)
  (let ((c (center i)))
    (/ (- (upper-bound i) c)
       c)))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;;; tests begin
(load "../testframe.scm")

;; make sure percent procedure produces the right tolerance
(assert= (percent (make-center-percent 10 0.125)) 0.125)
(assert= (percent (make-center-width 10 1.25)) 0.125)

;; make sure center and width are right
(assert= (center (make-center-percent 10 0.111)) 10)
(assert= (center (make-center-width 10 0.1)) 10)
(assert= (width (make-center-percent 10 0.1)) 1)
(assert= (width (make-center-width 10 1)) 1)

