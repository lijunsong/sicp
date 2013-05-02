(define (fib-new n)
  (define (fib-iter a b p q count)
    (cond ((= count 0) b)
          ((even? count)
           (fib-iter a
                     b
                     (+ (* p p) (* q q))
                     (+ (* q q) (* 2 p q))
                     (/ count 2)))
          (else (fib-iter (+ (* b q) (* a q) (* a p))
                          (+ (* b p) (* a q))
                          p
                          q
                          (- count 1)))))
  (fib-iter 1 0 0 1 n))

; for testing
(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))  
  (fib-iter 1 0 n))


;;; tests begin
(load "../testframe.scm")

(for-each (lambda (x)
            (assert= (fib x) (fib-new x)))
          '(1 2 3 4 5 6 7 8 9 10))
