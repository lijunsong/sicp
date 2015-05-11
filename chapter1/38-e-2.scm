(load "37-cont-frac.scm")
(load "../testframe.scm")
(let ((e-1
       (/ 1
          (cont-frac (lambda (i) 1.0)
                     (lambda (i)
                       (if (= (remainder i 3) 0)
                           (/ (* 2 i) 3)
                           1))
                     100)))
      (e 2.7182818284590452353602874713))
  (display (+ e-1 1))) ;Value: 2.7182818284590455
