;;; fibonacci number
(define (fibonacci x)
  ((lambda (n)
     ((lambda (fibo)
        (fibo fibo n))
      (lambda (f k)
        (cond ((= k 1) 1)
              ((= k 2) 1)
              (else (+ (f f (- k 1))
                       (f f (- k 2))))))))
   x))

;;; alternative definition of f
(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) false (ev? ev? od? (- n 1))))))

;;; tests begin
(load "../testframe.scm")
(assert= (fibonacci 1) 1)
(assert= (fibonacci 2) 1)
(assert= (fibonacci 3) 2)
(assert= (fibonacci 4) 3)
(assert= (fibonacci 5) 5)
(assert= (fibonacci 6) 8)

(asserteq? (f 10) true)
(asserteq? (f 11) false)


