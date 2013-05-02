(load "37-cont-frac.scm")

(define (tan-cf x k)
  (let ((xx (* x x)))
    (/ (cont-frac (lambda (i) (- 0 xx))
                  (lambda (i) (- (* 2 i) 1))
                  k)
       (- 0 x))))

(tan-cf (/ 3.1415926 4) 100)
;Value: .9999999732051038
; the built-in tan will produce
; 1 ]=> (tan (/ 3.1415926 4))
;Value: .9999999732051039

