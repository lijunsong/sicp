(define (cont-frac n d k)
  (define (rec i)
    (cond ((> i k) 0)
          (else
           (/ (n i)
              (+ (d i)
                 (rec (+ i 1)))))))
  (rec 1))

; iterative one
(define (cont-frac/iter n d k)
  (define (iter i result)
    (cond ((= i 0) result)
          (else
           (iter (- i 1) (/ (n i)
                            (+ (d i) result))))))
  (iter k 0))


;;; tests begin
(load "../testframe.scm")
(assert= (cont-frac/iter (lambda (i) 1.0)
                         (lambda (i) 1.0)
                         100)
         (cont-frac (lambda (i) 1.0)
                    (lambda (i) 1.0)
                    100))

(define (spot)
;;; spot the result
                                        ; 1/(golden ratio) = .6180339887498948

  (display (cont-frac (lambda (i) 1.0)
                      (lambda (i) 1.0)
                      10))
                                        ;Value: .6179775280898876

  (display (cont-frac (lambda (i) 1.0)
                      (lambda (i) 1.0)
                      11))
                                        ;Value: .6180555555555556

  (display (cont-frac (lambda (i) 1.0)
                      (lambda (i) 1.0)
                      14))
                                        ;Value: .6180327868852459
)
