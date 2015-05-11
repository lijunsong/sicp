(load "43-repeated.scm")

(define (smooth f)
  (define dx 0.00001)
  (lambda (x)
    (/ (+ (f (+ x dx))
          (f x)
          (f (- x dx)))
       3)))

(define (n-fold-smooth-f n f)
  (lambda (x)
    ((repeated smooth n) f) x))

;;; todo: add tests
