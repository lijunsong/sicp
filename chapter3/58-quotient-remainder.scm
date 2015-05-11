(load "stream.scm")

(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))


;;; tests begin
(load "../testframe.scm")

; 10.0 / 7 = 1.4285714285714286
(let ((get10/7 (expand 1 7 10)))
  (assert= (stream-car get10/7) 1)
  (assert= (stream-ref get10/7 1) 4)
  (assert= (stream-ref get10/7 2) 2)
  (assert= (stream-ref get10/7 6) 1))

; 30.0 / 8 = 3.75
(let ((get30/8 (expand 3 8 10)))
  (assert= (stream-ref get30/8 0) 3)
  (assert= (stream-ref get30/8 1) 7)
  (assert= (stream-ref get30/8 2) 5)
  (assert= (stream-ref get30/8 3) 0))


