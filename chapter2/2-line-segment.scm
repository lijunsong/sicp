;;; point
(define (make-point x y)
  (cons x y))
(define (x-point point)
  (car point))
(define (y-point point)
  (cdr point))

;;; segment
(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))


;;; operation on segment
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (midpoint-segment segment)
  (make-point (/ (+ (x-point (start-segment segment))
                    (x-point (end-segment segment)))
                 2)
              (/ (+ (y-point (start-segment segment))
                    (y-point (end-segment segment)))
                 2)))

;;; tests begin
(load "../testframe.scm")

;;; the midpoint of points (1, 3) and (4, 5) is (5/2, 4)
(assert-output-equal
 "\n(5/2,4)"
 (print-point (midpoint-segment (make-segment (make-point 1 3) (make-point 4 5)))))


