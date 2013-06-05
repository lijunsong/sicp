(load "stream.scm")

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (divisible? x y) (= (remainder x y) 0))

(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve
    (stream-filter
     (lambda (x)
       (not (divisible? x (stream-car stream))))
     (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))
