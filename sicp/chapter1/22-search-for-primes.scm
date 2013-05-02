;; prime?
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

;; timed prime?

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-time)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (current-time) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

;; search-for-primes starts

(define (current-time)
  (real-time-clock))

(define (search-for-primes s e)
  (cond ((> s e) 'done)
        ((even? s) (search-for-primes (+ 1 s) e))
        (else
         (begin (timed-prime-test s)
                (search-for-primes (+ 1 s) e)))))

(define search
  (lambda ()
    (search-for-primes 1000 1020)
                                        ; smallest primes are 1009 1013 1019
    (search-for-primes 10000 10040)
                                        ; smallest primes are 10007 10009 10037
    (search-for-primes 100000 100050)
                                        ; smallest primes are 100003 100019 100043
    (search-for-primes 1000000 1000040)
                                        ; smallest primes are 1000003 1000033 1000037
    (search-for-primes 10000000 10000200)))
