(load "fixed-point.scm")
(load "43-repeated.scm")

(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (power x n)
  (if (<= n 0) 1 (* x (power x (- n 1)))))

(define (nth-root-testing average-damping-times n)
  (lambda (x)
    (fixed-point ((repeated average-damp average-damping-times)
                  (lambda (y) (/ x (power y (- n 1)))))
                 1.0)))

;; the tests are shown below:

; ((nth-root-testing 0  2) 1024) => infinite loop
; ((nth-root-testing 1  2) 1024) => 32.0000000000008
; ((nth-root-testing 1  3) 1024) => 10.079366187684354
; ((nth-root-testing 1  4) 1024) => infinite loop
; ((nth-root-testing 2  4) 1024) => 5.65685424949238
; ((nth-root-testing 2  5) 1024) => 4.000001066833838
; ((nth-root-testing 2  6) 1024) => 3.1748038863282293
; ((nth-root-testing 2  7) 1024) => 2.691796716281978
; ((nth-root-testing 2  8) 1024) => infinite loop
; ((nth-root-testing 3  8) 1024) => 2.3784142300068343
; ((nth-root-testing 3  9) 1024) => 2.1601204050836174
; ((nth-root-testing 3 10) 1024) => 2.000001183010332
; ((nth-root-testing 3 11) 1024) => 1.8778630537548755
; ((nth-root-testing 3 12) 1024) => 1.7817995120073165
; ((nth-root-testing 3 13) 1024) => 1.7043576123984447
; ((nth-root-testing 3 14) 1024) => 1.6406664288586745
; ((nth-root-testing 3 15) 1024) => 1.587396859718543
; ((nth-root-testing 3 16) 1024) => infinite loop
; ((nth-root-testing 4 16) 1024) => 1.5422108254105882
; ...
; ((nth-root-testing 4 31) 1024) => 1.2505702303239892
; ((nth-root-testing 4 32) 1024) => infinite loop


;; nth-root 
(define (nth-root n)
  (lambda (x)
    (fixed-point ((repeated average-damp (ceiling (sqrt n)))
                  (lambda (y) (/ x (power y (- n 1)))))
                 1.0)))

(define (new-sqrt x)
  ((nth-root 2) x))

; (new-sqrt 4) => 1.999993326908296
