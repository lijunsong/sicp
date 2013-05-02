(define (f/rec n)
  (cond ((< n 3) n)
        (else
         (+ (f/rec (- n 1))
            (* 2 (f/rec (- n 2)))
            (* 3 (f/rec (- n 3)))))))

; iterative procedure
(define (f/iter n)
  (define (iter fn-1 fn-2 fn-3 counter)
    (cond ((< n 3) n)
          ((> counter n) fn-1)
          (else
           (iter (+ fn-1 (* 2 fn-2) (* 3 fn-3))
                 fn-1 fn-2
                 (+ counter 1)))))
  (iter 2 1 0 3))

;;; tests begin
(load "../testframe.scm")

(for-each (lambda (x)
            (assert= (f/rec x) (f/iter x)))
          '(1 2 3 4 5 6 7 8 9 10))
